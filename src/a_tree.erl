-module(a_tree).

-export([new/1, insert/3, search/2, delete/2]).

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

-spec new(Attributes :: [attribute()]) -> {ok, atree()} | {error, term()}.
new(_Attributes) ->
    ?NOT_LOADED.

-spec insert(ATree :: atree(), Id :: user_id(), Expression :: binary()) ->
    ok | {error, term()}.
insert(_ATree, _Id, _Expression) ->
    ?NOT_LOADED.

-spec search(ATree :: atree(), Event :: event()) ->
    {ok, Matches :: [user_id()]} | {error, term()}.
search(_ATree, _Event) ->
    ?NOT_LOADED.

-spec delete(ATree :: atree(), Id :: user_id()) -> ok.
delete(_ATree, _Id) ->
    ?NOT_LOADED.

%%%===================================================================
%%% NIF
%%%===================================================================

init() ->
    ?load_nif_from_crate(a_tree, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
