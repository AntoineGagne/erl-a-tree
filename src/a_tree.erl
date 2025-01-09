-module(a_tree).

-moduledoc """
A module for interacting with an `ATree`.

# Examples

```erlang
1> {ok, Tree} = a_tree:new([
    {boolean, <<"private">>},
    {string, <<"country">>},
    {integer, <<"exchange_id">>},
    {string, <<"city">>},
    {string_list, <<"deals">>},
    {integer_list, <<"segment_ids">>}
]).
2> ok = a_tree:insert(Tree, 1, <<"exchange_id = 1 and not private and deals one of [\"deal-1\", \"deal-2\"]">>).
3> ok = a_tree:insert(Tree, 2, <<"exchange_id = 1 and not private and deals one of [\"deal-2\", \"deal-3\"] and segment_ids one of [1, 2, 3, 4]">>).
4> ok = a_tree:insert(Tree, 3, <<"exchange_id = 1 and not private and deals one of [\"deal-2\", \"deal-3\"] and segment_ids one of [5, 6, 7, 8] and country in [\"CA\", \"US\"]">>).
5> {ok, Results} = a_tree:search([
    {<<"private">>, false},
    {<<"exchange_id">>, 1},
    {<<"deals">>, [<<"deal-1">>, <<"deal-3">>]},
    {<<"segment_ids">>, [2, 3]},
    {<<"country">>, <<"CA">>}
]).
```
""".

-export([
    new/1,
    insert/3,
    search/2,
    delete/2,
    to_graphviz/1
]).

-include("cargo.hrl").

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).

-type attribute() :: {attribute_type(), attribute_name()}.
-type event() :: [{attribute_name(), attribute_value()}].
-type attribute_type() :: boolean | integer | string | integer_list | string_list.
-type attribute_name() :: binary().
-type attribute_value() :: boolean() | binary() | integer() | [integer()] | [binary()].
-type user_id() :: non_neg_integer().

-opaque atree() :: reference().

-export_type([
    attribute/0,
    attribute_type/0,
    attribute_name/0,
    atree/0,
    event/0,
    user_id/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-doc """
Create a new `t:atree/0`.

The attributes must all be different otherwise an error will be returned.
""".
-spec new(Attributes :: [attribute()]) -> {ok, atree()} | {error, term()}.
new(_Attributes) ->
    ?NOT_LOADED.

-doc """
Insert a new arbitrary boolean expression inside the `t:atree/0` with the specified `t:user_id/0`.

The boolean expression must contain defined attributes with the correct types otherwise an error will be returned.
""".
-spec insert(ATree :: atree(), Id :: user_id(), Expression :: binary()) ->
    ok | {error, term()}.
insert(_ATree, _Id, _Expression) ->
    ?NOT_LOADED.

-doc """
Search for boolean expressions that match the specified event.
""".
-spec search(ATree :: atree(), Event :: event()) ->
    {ok, Matches :: [user_id()]} | {error, term()}.
search(_ATree, _Event) ->
    ?NOT_LOADED.

-doc """
Remove the specified boolean expression from the `t:atree/0`.
""".
-spec delete(ATree :: atree(), Id :: user_id()) -> ok.
delete(_ATree, _Id) ->
    ?NOT_LOADED.

-doc """
Export the `t:atree/0` to Graphviz format.
""".
-spec to_graphviz(ATree :: atree()) -> {ok, binary()} | {error, term()}.
to_graphviz(_ATree) ->
    ?NOT_LOADED.

%%%===================================================================
%%% NIF
%%%===================================================================

init() ->
    ?load_nif_from_crate(a_tree, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
