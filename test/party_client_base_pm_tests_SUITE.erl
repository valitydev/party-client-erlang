-module(party_client_base_pm_tests_SUITE).

-include("party_domain_fixtures.hrl").

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_and_get_test/1]).
-export([party_errors_test/1]).
-export([party_operations_test/1]).
-export([contract_create_and_get_test/1]).
-export([shop_create_and_get_test/1]).
-export([shop_operations_test/1]).
-export([claim_operations_test/1]).
-export([get_revision_test/1]).

-export([compute_provider_ok/1]).
-export([compute_provider_not_found/1]).
-export([compute_provider_terminal_terms_ok/1]).
-export([compute_provider_terminal_terms_not_found/1]).
-export([compute_globals_ok/1]).
-export([compute_routing_ruleset_ok/1]).
-export([compute_routing_ruleset_unreducable/1]).
-export([compute_routing_ruleset_not_found/1]).

%% Internal types

-type test_entry() :: atom() | {group, atom()}.
-type group() :: {atom(), [Opts :: atom()], [test_entry()]}.
-type config() :: [{atom(), any()}].

-define(WRONG_DMT_OBJ_ID, 99999).

%% CT description

-spec all() -> [test_entry()].
all() ->
    [
        {group, party_management_api},
        {group, party_management_compute_api}
    ].

-spec groups() -> [group()].
groups() ->
    [
        {party_management_api, [parallel], [
            create_and_get_test,
            party_errors_test,
            party_operations_test,
            contract_create_and_get_test,
            shop_create_and_get_test,
            shop_operations_test,
            claim_operations_test,
            get_revision_test
        ]},
        {party_management_compute_api, [parallel], [
            compute_provider_ok,
            compute_provider_not_found,
            compute_provider_terminal_terms_ok,
            compute_provider_terminal_terms_not_found,
            compute_globals_ok,
            compute_routing_ruleset_ok,
            compute_routing_ruleset_unreducable,
            compute_routing_ruleset_not_found
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({'scoper_woody_event_handler', 'handle_event', '_'}, x),
    AppConfig = [
        {dmt_client, [
            % milliseconds
            {cache_update_interval, 5000},
            {max_cache_size, #{
                elements => 1,
                % 2Kb
                memory => 2048
            }},
            {service_urls, #{
                'Repository' => <<"http://dominant:8022/v1/domain/repository">>,
                'RepositoryClient' => <<"http://dominant:8022/v1/domain/repository_client">>
            }}
        ]},
        {party_client, []}
    ],
    Apps = lists:flatten([genlib_app:start_application_with(A, C) || {A, C} <- AppConfig]),
    {ok, Revision} = init_domain(),
    Client = party_client:create_client(),
    {ok, ClientPid} = party_client:start_link(Client),
    true = erlang:unlink(ClientPid),
    [{apps, Apps}, {client, Client}, {client_pid, ClientPid}, {test_id, genlib:to_binary(Revision)} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(C) ->
    true = erlang:exit(conf(client_pid, C), shutdown),
    genlib_app:stop_unload_applications(proplists:get_value(apps, C)).

-spec init_per_group(atom(), config()) -> config().
init_per_group(Group, Config) ->
    [{test_id, genlib:to_binary(Group)} | Config].

-spec end_per_group(atom(), config()) -> ok.
end_per_group(_Group, _Config) ->
    ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(Name, Config) ->
    [{test_id, genlib:to_binary(Name)} | Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_Name, _Config) ->
    ok.

%% Tests

-spec create_and_get_test(config()) -> any().
create_and_get_test(C) ->
    {ok, PartyId, Client, Context} = test_init_info(C),
    ContactInfo = #domain_PartyContactInfo{registration_email = PartyId},
    ok = party_client_thrift:create(PartyId, make_party_params(ContactInfo), Client, Context),
    {ok, Party} = party_client_thrift:get(PartyId, Client, Context),
    #domain_Party{id = PartyId, contact_info = ContactInfo} = Party.

-spec party_errors_test(config()) -> any().
party_errors_test(C) ->
    {ok, PartyId, Client, Context} = test_init_info(C),
    ContactInfo = #domain_PartyContactInfo{registration_email = PartyId},
    PartyParams = make_party_params(ContactInfo),
    ok = party_client_thrift:create(PartyId, PartyParams, Client, Context),
    {error, #payproc_PartyExists{}} = party_client_thrift:create(PartyId, PartyParams, Client, Context),
    {error, #payproc_PartyNotFound{}} = party_client_thrift:get(<<"not_exists">>, Client, Context),
    {error, #payproc_InvalidPartyRevision{}} =
        party_client_thrift:checkout(PartyId, {revision, 100500}, Client, Context),
    {error, #payproc_InvalidPartyStatus{}} = party_client_thrift:activate(PartyId, Client, Context),
    ok.

-spec party_operations_test(config()) -> any().
party_operations_test(C) ->
    {ok, _TestId, Client, Context} = test_init_info(C),
    {ok, PartyId} = create_party(C),
    ok = party_client_thrift:suspend(PartyId, Client, Context),
    ok = party_client_thrift:activate(PartyId, Client, Context),
    ok = party_client_thrift:block(PartyId, <<"block_test">>, Client, Context),
    ok = party_client_thrift:unblock(PartyId, <<"unblock_test">>, Client, Context),
    {ok, #{}} = party_client_thrift:get_meta(PartyId, Client, Context),
    MetadataNs = <<"metadata">>,
    Metadata = {str, <<"cool_stuff">>},
    ok = party_client_thrift:set_metadata(PartyId, MetadataNs, Metadata, Client, Context),
    {ok, Metadata} = party_client_thrift:get_metadata(PartyId, MetadataNs, Client, Context),
    ok = party_client_thrift:remove_metadata(PartyId, MetadataNs, Client, Context),
    ok.

-spec contract_create_and_get_test(config()) -> any().
contract_create_and_get_test(C) ->
    {ok, _TestId, Client, Context} = test_init_info(C),
    {ok, PartyId} = create_party(C),
    {ok, ContractId} = create_contract(PartyId, C),
    {ok, Contract} = party_client_thrift:get_contract(PartyId, ContractId, Client, Context),
    #domain_Contract{id = ContractId} = Contract,
    Timestamp = genlib_rfc3339:format(genlib_time:unow() + 10, millisecond),
    {ok, DomainRevision} = dmt_client_cache:update(),
    {ok, PartyRevision} = party_client_thrift:get_revision(PartyId, Client, Context),
    Varset = #payproc_ComputeContractTermsVarset{},
    {ok, _Terms} = party_client_thrift:compute_contract_terms(
        PartyId,
        ContractId,
        Timestamp,
        {revision, PartyRevision},
        DomainRevision,
        Varset,
        Client,
        Context
    ).

-spec shop_create_and_get_test(config()) -> any().
shop_create_and_get_test(C) ->
    {ok, _TestId, Client, Context} = test_init_info(C),
    {ok, PartyId} = create_party(C),
    {ok, ContractId} = create_contract(PartyId, C),
    {ok, ShopId} = create_shop(PartyId, ContractId, C),
    {ok, Shop} = party_client_thrift:get_shop(PartyId, ShopId, Client, Context),
    #domain_Shop{id = ShopId} = Shop,
    Timestamp = genlib_rfc3339:format(genlib_time:unow() + 10, millisecond),
    {ok, PartyRevision} = party_client_thrift:get_revision(PartyId, Client, Context),
    PartyRevisionParam = {revision, PartyRevision},
    Varset = #payproc_ComputeShopTermsVarset{},
    {ok, _Terms} =
        party_client_thrift:compute_shop_terms(PartyId, ShopId, Timestamp, PartyRevisionParam, Varset, Client, Context).

-spec shop_operations_test(config()) -> any().
shop_operations_test(C) ->
    {ok, _TestId, Client, Context} = test_init_info(C),
    {ok, PartyId} = create_party(C),
    {ok, ContractId} = create_contract(PartyId, C),
    {ok, ShopId} = create_shop(PartyId, ContractId, C),
    ok = party_client_thrift:suspend_shop(PartyId, ShopId, Client, Context),
    ok = party_client_thrift:activate_shop(PartyId, ShopId, Client, Context),
    ok = party_client_thrift:block_shop(PartyId, ShopId, <<"block_test">>, Client, Context),
    ok = party_client_thrift:unblock_shop(PartyId, ShopId, <<"unblock_test">>, Client, Context),
    {ok, #domain_ShopAccount{settlement = AccountID}} =
        party_client_thrift:get_shop_account(PartyId, ShopId, Client, Context),
    {ok, #payproc_ShopContract{
        shop = #domain_Shop{id = ShopId},
        contract = #domain_Contract{id = ContractId}
    }} =
        party_client_thrift:get_shop_contract(PartyId, ShopId, Client, Context),
    {ok, _ShopAccount} = party_client_thrift:get_account_state(PartyId, AccountID, Client, Context).

-spec claim_operations_test(config()) -> any().
claim_operations_test(C) ->
    {ok, TestId, Client, Context} = test_init_info(C),
    {ok, PartyId} = create_party(C),
    {ok, _ContractId} = create_contract(PartyId, C),
    {ok, [ContractClaim]} = party_client_thrift:get_claims(PartyId, Client, Context),
    #payproc_Claim{id = ClaimId, revision = _Revision} = ContractClaim,
    {ok, ContractClaim} = party_client_thrift:get_claim(PartyId, ClaimId, Client, Context),
    ContractParams = #payproc_ContractParams{
        contractor = make_battle_ready_contractor(),
        template = undefined,
        payment_institution = #domain_PaymentInstitutionRef{id = 2}
    },
    NewContractId = <<TestId/binary, ".new_contract">>,
    Changeset = [
        {contract_modification, #payproc_ContractModificationUnit{
            id = NewContractId,
            modification = {creation, ContractParams}
        }}
    ],
    {ok, NewClaim0} = party_client_thrift:create_claim(PartyId, Changeset, Client, Context),
    #payproc_Claim{id = NewClaimId, revision = NewRevision0} = NewClaim0,
    ok = party_client_thrift:update_claim(PartyId, NewClaimId, NewRevision0, [], Client, Context),
    {ok, #payproc_Claim{revision = NewRevision1}} =
        party_client_thrift:get_claim(PartyId, NewClaimId, Client, Context),
    ok = party_client_thrift:deny_claim(PartyId, NewClaimId, NewRevision1, <<"deny_test">>, Client, Context),
    {ok, #payproc_Claim{revision = NewRevision2}} =
        party_client_thrift:get_claim(PartyId, NewClaimId, Client, Context),
    {error, #payproc_InvalidClaimStatus{}} =
        party_client_thrift:revoke_claim(PartyId, NewClaimId, NewRevision2, <<"revoke_test">>, Client, Context),
    {ok, [ContractClaim, _NewClaim]} = party_client_thrift:get_claims(PartyId, Client, Context).

-spec get_revision_test(config()) -> any().
get_revision_test(C) ->
    {ok, PartyId, Client, Context} = test_init_info(C),
    ContactInfo = #domain_PartyContactInfo{registration_email = PartyId},
    ok = party_client_thrift:create(PartyId, make_party_params(ContactInfo), Client, Context),
    {ok, Party} = party_client_thrift:get(PartyId, Client, Context),
    {ok, R1} = party_client_thrift:get_revision(PartyId, Client, Context),
    #domain_Party{id = PartyId, contact_info = ContactInfo, revision = R1} = Party,
    {ok, []} = party_client_thrift:get_claims(PartyId, Client, Context),
    ContractParams = #payproc_ContractParams{
        contractor = make_battle_ready_contractor(),
        template = undefined,
        payment_institution = #domain_PaymentInstitutionRef{id = 2}
    },
    NewContractId = <<PartyId/binary, ".new_contract">>,
    Changeset = [
        {contract_modification, #payproc_ContractModificationUnit{
            id = NewContractId,
            modification = {creation, ContractParams}
        }}
    ],
    {ok, Claim} = party_client_thrift:create_claim(PartyId, Changeset, Client, Context),
    #payproc_Claim{id = ClaimId, revision = Revision} = Claim,
    {ok, R1} = party_client_thrift:get_revision(PartyId, Client, Context),
    ok = party_client_thrift:accept_claim(PartyId, ClaimId, Revision, Client, Context),
    {ok, R2} = party_client_thrift:get_revision(PartyId, Client, Context),
    R2 = R1 + 1.

-spec compute_provider_ok(config()) -> any().
compute_provider_ok(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{
        currency = ?cur(<<"RUB">>)
    },
    CashFlow = make_test_cashflow(),
    {ok, #domain_Provider{
        terms = #domain_ProvisionTermSet{
            payments = #domain_PaymentsProvisionTerms{
                cash_flow = {value, [CashFlow]}
            },
            recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
                cash_value = {value, ?cash(1000, <<"RUB">>)}
            }
        }
    }} = party_client_thrift:compute_provider(?prv(1), DomainRevision, Varset, Client, Context).

-spec compute_provider_not_found(config()) -> any().
compute_provider_not_found(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    {error, #payproc_ProviderNotFound{}} =
        party_client_thrift:compute_provider(
            ?prv(2),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        ).

-spec compute_provider_terminal_terms_ok(config()) -> any().
compute_provider_terminal_terms_ok(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{
        currency = ?cur(<<"RUB">>)
    },
    CashFlow = make_test_cashflow(),
    PaymentMethods = ?ordset([?pmt_bank_card(visa)]),
    {ok, #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            cash_flow = {value, [CashFlow]},
            payment_methods = {value, PaymentMethods}
        }
    }} = party_client_thrift:compute_provider_terminal_terms(
        ?prv(1),
        ?trm(1),
        DomainRevision,
        Varset,
        Client,
        Context
    ).

-spec compute_provider_terminal_terms_not_found(config()) -> any().
compute_provider_terminal_terms_not_found(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    {error, #payproc_TerminalNotFound{}} =
        party_client_thrift:compute_provider_terminal_terms(
            ?prv(1),
            ?trm(?WRONG_DMT_OBJ_ID),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        ),
    {error, #payproc_ProviderNotFound{}} =
        party_client_thrift:compute_provider_terminal_terms(
            ?prv(2),
            ?trm(1),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        ),
    {error, #payproc_ProviderNotFound{}} =
        party_client_thrift:compute_provider_terminal_terms(
            ?prv(2),
            ?trm(?WRONG_DMT_OBJ_ID),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        ).

-spec compute_globals_ok(config()) -> any().
compute_globals_ok(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{},
    {ok, #domain_Globals{
        external_account_set = {value, ?eas(1)}
    }} = party_client_thrift:compute_globals(DomainRevision, Varset, Client, Context).

-spec compute_routing_ruleset_ok(config()) -> any().
compute_routing_ruleset_ok(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{
        party_id = <<"67890">>
    },
    {ok, #domain_RoutingRuleset{
        name = <<"Rule#1">>,
        decisions =
            {candidates, [
                #domain_RoutingCandidate{
                    terminal = ?trm(2),
                    allowed = {constant, true}
                },
                #domain_RoutingCandidate{
                    terminal = ?trm(3),
                    allowed = {constant, true}
                },
                #domain_RoutingCandidate{
                    terminal = ?trm(1),
                    allowed = {constant, true}
                }
            ]}
    }} = party_client_thrift:compute_routing_ruleset(?ruleset(1), DomainRevision, Varset, Client, Context).

-spec compute_routing_ruleset_unreducable(config()) -> any().
compute_routing_ruleset_unreducable(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{},
    {ok, #domain_RoutingRuleset{
        name = <<"Rule#1">>,
        decisions =
            {delegates, [
                #domain_RoutingDelegate{
                    allowed = {condition, {party, #domain_PartyCondition{id = <<"12345">>}}},
                    ruleset = ?ruleset(2)
                },
                #domain_RoutingDelegate{
                    allowed = {condition, {party, #domain_PartyCondition{id = <<"67890">>}}},
                    ruleset = ?ruleset(3)
                },
                #domain_RoutingDelegate{
                    allowed = {constant, true},
                    ruleset = ?ruleset(4)
                }
            ]}
    }} = party_client_thrift:compute_routing_ruleset(?ruleset(1), DomainRevision, Varset, Client, Context).

-spec compute_routing_ruleset_not_found(config()) -> any().
compute_routing_ruleset_not_found(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    {error, #payproc_RuleSetNotFound{}} =
        (catch party_client_thrift:compute_routing_ruleset(
            ?ruleset(5),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        )).

%% Internal functions

%% Environment confirators

-spec init_domain() -> {ok, integer()}.
init_domain() ->
    {ok, _} = dmt_client_cache:update(),
    ok = party_domain_fixtures:cleanup(),
    {ok, _} = dmt_client_cache:update(),
    ok = party_domain_fixtures:apply_domain_fixture(),
    {ok, _Revision} = dmt_client_cache:update().

create_party(C) ->
    {ok, TestId, Client, Context} = test_init_info(C),
    PartyId = <<TestId/binary, ".party">>,
    ContactInfo = #domain_PartyContactInfo{registration_email = <<TestId/binary, "@example.com">>},
    ok = party_client_thrift:create(PartyId, make_party_params(ContactInfo), Client, Context),
    {ok, PartyId}.

create_contract(PartyId, C) ->
    {ok, TestId, Client, Context} = test_init_info(C),
    ContractParams = #payproc_ContractParams{
        contractor = make_battle_ready_contractor(),
        template = undefined,
        payment_institution = #domain_PaymentInstitutionRef{id = 2}
    },
    PayoutToolParams = make_battle_ready_payout_tool_params(),
    ContractId = <<TestId/binary, ".contract">>,
    Changeset = [
        {contract_modification, #payproc_ContractModificationUnit{
            id = ContractId,
            modification = {creation, ContractParams}
        }},
        {contract_modification, #payproc_ContractModificationUnit{
            id = ContractId,
            modification =
                {payout_tool_modification, #payproc_PayoutToolModificationUnit{
                    payout_tool_id = <<"1">>,
                    modification = {creation, PayoutToolParams}
                }}
        }}
    ],
    create_and_accept_claim(PartyId, Changeset, Client, Context),
    {ok, ContractId}.

create_shop(PartyId, ContractId, C) ->
    {ok, TestId, Client, Context} = test_init_info(C),
    ShopId = <<TestId/binary, ".shop">>,
    Currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>},
    Details = #domain_ShopDetails{
        name = <<"THRIFT SHOP">>,
        description = <<"Hot. Fancy. Almost free.">>
    },
    Params = #payproc_ShopParams{
        category = #domain_CategoryRef{id = 2},
        location = {url, <<"https://somename.somedomain/p/123?redirect=1">>},
        details = Details,
        contract_id = ContractId,
        payout_tool_id = get_first_payout_tool_id(PartyId, ContractId, Client, Context)
    },
    ShopAccountParams = #payproc_ShopAccountParams{currency = Currency},
    Changeset = [
        {shop_modification, #payproc_ShopModificationUnit{
            id = ShopId,
            modification = {creation, Params}
        }},
        {shop_modification, #payproc_ShopModificationUnit{
            id = ShopId,
            modification = {shop_account_creation, ShopAccountParams}
        }}
    ],
    create_and_accept_claim(PartyId, Changeset, Client, Context),
    {ok, ShopId}.

create_and_accept_claim(PartyId, Changeset, Client, Context) ->
    {ok, Claim} = party_client_thrift:create_claim(PartyId, Changeset, Client, Context),
    #payproc_Claim{id = ClaimId, revision = Revision} = Claim,
    ok = party_client_thrift:accept_claim(PartyId, ClaimId, Revision, Client, Context).

%% Config helpers

-spec get_test_id(config()) -> binary().
get_test_id(Config) ->
    AllId = lists:reverse(proplists:append_values(test_id, Config)),
    erlang:iolist_to_binary([[<<".">> | I] || I <- AllId]).

conf(Key, Config) ->
    proplists:get_value(Key, Config).

%% Domain objects constructors

make_party_params(ContactInfo) ->
    #payproc_PartyParams{contact_info = ContactInfo}.

create_context() ->
    party_client:create_context().

test_init_info(C) ->
    PartyId = get_test_id(C),
    Client = conf(client, C),
    Context = create_context(),
    {ok, PartyId, Client, Context}.

-spec make_battle_ready_contractor() -> dmsl_domain_thrift:'Contractor'().
make_battle_ready_contractor() ->
    BankAccount = #domain_RussianBankAccount{
        account = <<"4276300010908312893">>,
        bank_name = <<"SomeBank">>,
        bank_post_account = <<"123129876">>,
        bank_bik = <<"66642666">>
    },
    {legal_entity,
        {russian_legal_entity, #domain_RussianLegalEntity{
            registered_name = <<"Hoofs & Horns OJSC">>,
            registered_number = <<"1234509876">>,
            inn = <<"1213456789012">>,
            actual_address = <<"Nezahualcoyotl 109 Piso 8, Centro, 06082, MEXICO">>,
            post_address = <<"NaN">>,
            representative_position = <<"Director">>,
            representative_full_name = <<"Someone">>,
            representative_document = <<"100$ banknote">>,
            russian_bank_account = BankAccount
        }}}.

-spec make_battle_ready_payout_tool_params() -> dmsl_payproc_thrift:'PayoutToolParams'().
make_battle_ready_payout_tool_params() ->
    #payproc_PayoutToolParams{
        currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>},
        tool_info =
            {russian_bank_account, #domain_RussianBankAccount{
                account = <<"4276300010908312893">>,
                bank_name = <<"SomeBank">>,
                bank_post_account = <<"123129876">>,
                bank_bik = <<"66642666">>
            }}
    }.

-spec make_test_cashflow() -> dmsl_domain_thrift:'CashFlowPosting'().
make_test_cashflow() ->
    ?cfpost(
        {system, settlement},
        {provider, settlement},
        {product,
            {min_of,
                ?ordset([
                    ?fixed(10, <<"RUB">>),
                    ?share(5, 100, operation_amount, round_half_towards_zero)
                ])}}
    ).

%% Other helpers

-spec get_first_payout_tool_id(binary(), binary(), party_client:client(), party_client:context()) ->
    dmsl_domain_thrift:'PayoutToolID'().
get_first_payout_tool_id(PartyId, ContractId, Client, Context) ->
    {ok, Contract} = party_client_thrift:get_contract(PartyId, ContractId, Client, Context),
    #domain_Contract{payout_tools = PayoutTools} = Contract,
    case PayoutTools of
        [Tool | _] ->
            Tool#domain_PayoutTool.id;
        [] ->
            error(no_payout_tools)
    end.
