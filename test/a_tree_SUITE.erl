-module(a_tree_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(SOME_ATTRIBUTES,
        [{boolean, <<"private">>},
         {string, <<"country">>},
         {integer, <<"exchange_id">>},
         {string, <<"city">>},
         {string_list, <<"deals">>},
         {integer_list, <<"segment_ids">>}]).
-define(AN_ID, 1).
-define(AN_EXPRESSION,
        <<"exchange_id = 1 and not private and deals one of [\"deal-1\", "
          "\"deal-2\"]">>).
-define(AN_INVALID_EXPRESSION, <<"exchange_id = 1 and deals one of [\"deal-1\"">>).
-define(AN_EXPRESSION_WITH_A_NON_EXISTING_ATTRIBUTE, <<"a_string = \"test\"">>).
-define(SOME_VALID_EXPRESSIONS_AND_IDS,
        [{?AN_ID,
          <<"exchange_id = 1 and not private and deals one of [\"deal-1\", "
            "\"deal-2\"]">>},
         {2,
          <<"exchange_id = 1 and not private and deals one of [\"deal-2\", "
            "\"deal-3\"]">>},
         {3,
          <<"exchange_id = 1 and not private and deals one of [\"deal-2\", "
            "\"deal-3\"] and segment_ids one of [1, 2, 3, 4]">>},
         {4,
          <<"exchange_id = 1 and not private and deals one of [\"deal-2\", "
            "\"deal-3\"] and segment_ids one of [5, 6, 7, 8] and country "
            "in [\"CA\", \"US\"]">>}]).
-define(AN_EVENT,
        [{<<"private">>, false},
         {<<"exchange_id">>, 1},
         {<<"deals">>, [<<"deal-1">>, <<"deal-3">>]},
         {<<"segment_ids">>, [2, 3]},
         {<<"country">>, <<"CA">>}]).

all() ->
    [{group, creation}, {group, insertion}, {group, search}, {group, deletion}].

groups() ->
    [{creation,
      [parallel],
      [can_create_an_empty_tree,
       can_create_a_tree,
       return_an_error_when_creating_a_tree_with_duplicate_attributes]},
     {insertion,
      [parallel],
      [can_insert_a_valid_expression, return_an_error_when_inserting_an_invalid_expression]},
     {search, [parallel], [can_search_a_tree]},
     {deletion,
      [parallel],
      [can_delete_an_expression_from_a_tree, deleting_a_non_existing_expression_ignores_it]}].

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Name, Config) ->
    GroupProperties = proplists:get_value(tc_group_properties, Config, []),
    case proplists:get_value(name, GroupProperties) of
        creation ->
            Config;
        _ ->
            %% To prevent each tests from interfering with each other
            {ok, Tree} = a_tree:new(?SOME_ATTRIBUTES),
            [{tree, Tree} | Config]
    end.

end_per_testcase(_Name, Config) ->
    Config.

%%%===================================================================
%%% Test cases
%%%===================================================================

can_create_an_empty_tree(_Config) ->
    ?assertMatch({ok, _}, a_tree:new([])).

can_create_a_tree(_Config) ->
    ?assertMatch({ok, _}, a_tree:new([{boolean, <<"private">>}])).

return_an_error_when_creating_a_tree_with_duplicate_attributes(_Config) ->
    ?assertMatch({error, _},
                 a_tree:new([{boolean, <<"private">>}, {integer, <<"private">>}])).

can_insert_a_valid_expression(Config) ->
    Tree = proplists:get_value(tree, Config),

    ?assertEqual(ok, a_tree:insert(Tree, ?AN_ID, ?AN_EXPRESSION)).

return_an_error_when_inserting_an_invalid_expression(Config) ->
    Tree = proplists:get_value(tree, Config),

    ?assertMatch({error, _}, a_tree:insert(Tree, ?AN_ID, ?AN_INVALID_EXPRESSION)).

return_an_error_when_inserting_an_expression_that_refers_to_a_non_existing_attribute(Config) ->
    Tree = proplists:get_value(tree, Config),

    ?assertMatch({error, _},
                 a_tree:insert(Tree, ?AN_ID, ?AN_EXPRESSION_WITH_A_NON_EXISTING_ATTRIBUTE)).

can_search_a_tree(Config) ->
    Tree = proplists:get_value(tree, Config),
    lists:foreach(fun({Id, Expression}) -> ok = a_tree:insert(Tree, Id, Expression) end,
                  ?SOME_VALID_EXPRESSIONS_AND_IDS),

    Results = a_tree:search(Tree, ?AN_EVENT),

    ?assertMatch({ok, _}, Results),
    {ok, Matches} = Results,
    ?assertEqual([1, 2, 3], lists:sort(Matches)).

can_delete_an_expression_from_a_tree(Config) ->
    Tree = proplists:get_value(tree, Config),
    lists:foreach(fun({Id, Expression}) -> ok = a_tree:insert(Tree, Id, Expression) end,
                  ?SOME_VALID_EXPRESSIONS_AND_IDS),

    Results = a_tree:search(Tree, ?AN_EVENT),
    ?assertMatch({ok, _}, Results),
    {ok, Matches} = Results,
    ?assertEqual([1, 2, 3], lists:sort(Matches)),

    ok = a_tree:delete(Tree, 2),
    Results2 = a_tree:search(Tree, ?AN_EVENT),
    ?assertMatch({ok, _}, Results2),
    {ok, Matches2} = Results2,
    ?assertEqual([1, 3], Matches2).

deleting_a_non_existing_expression_ignores_it(Config) ->
    Tree = proplists:get_value(tree, Config),

    ?assertEqual(ok, a_tree:delete(Tree, ?AN_ID)).

can_export_to_graphviz_format(Config) ->
    Tree = proplists:get_value(tree, Config),

    ?assertMatch({ok, _}, a_tree:to_graphviz(Tree)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
