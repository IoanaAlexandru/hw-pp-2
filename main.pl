% LIST CHECKS
% there's an element [E, _] in the list
member_first(E, [[E, _]|_]).
member_first(E, [[_, _]|T]) :-
    member_first(E, T).

% there's an element [_, E] in the list
member_second(E, [[_, E]|_]).
member_second(E, [[_, _]|T]) :-
    member_second(E, T).

% GRAPH CHECKS
% X is a node of graph [V, E]
node(X, [V, _]) :-
    member_first(X, V).

% X is a colour of graph [V, E]
colour(X, [V, _]) :-
    member_second(X, V).

% X->Y is an edge in graph [V, E]
edge(X, Y, [_, E]) :-
    member([X, Y], E).

% Colour is the colour of node N from given node list
get_colour([[N, Colour]|_], N, Colour) :- !.
get_colour([_|T], N, Colour) :- get_colour(T, N, Colour).

% FORMULAS
valid.

future([V, E], [H|T], Colour) :-
    get_colour(V, H, Colour);
    future([V, E], T, Colour).

global(_, [], _).
global([V, E], [H|T], Colour) :-
    get_colour(V, H, Colour),
    global([V, E], T, Colour).

until([V, _], [H], Colour1, _) :-
    get_colour(V, H, Colour1).
until([V, _], [H|_], _, Colour2) :-
    get_colour(V, H, Colour2).
until([V, E], [H|T], Colour1, Colour2) :-
    get_colour(V, H, Colour1),
    until([V, E], T, Colour1, Colour2).

% PATH FINDERS
% Path is a path in Graph from X to Y, in reverse order
% If there's an edge from X to Y, add it to path
path(X, Y, Graph, Visited, [Y|Visited]) :-
    edge(X, Y, Graph),
    not(member(Y, Visited)).
% If there's no edge from X to Y, find intermediary edge
path(X, Y, Graph, Visited, Path) :-
    edge(X, Z, Graph),                     % there exists an edge X->Z
    not(member(Z, Visited)),               % Z was not visited already
    path(Z, Y, Graph, [Z|Visited], Path).  % find path from Z to Y

% Path is a path in Graph from Src to Dest
get_paths(Src, Dest, Graph, Path) :-
    node(Src, Graph),                         % Src is a valid node in Graph
    node(Dest, Graph),                        % Dest is a valid node in Graph
    path(Src, Dest, Graph, [Src], Preverse),  % find path from Src to Dest
    reverse(Preverse, Path).                  % reverse found path

get_paths(Src, Dest, Graph, valid, Path) :-
    get_paths(Src, Dest, Graph, Path).
get_paths(Src, Dest, Graph, future(Colour), Path) :-
    get_paths(Src, Dest, Graph, Path),
    future(Graph, Path, Colour).
get_paths(Src, Dest, Graph, global(Colour), Path) :-
    get_paths(Src, Dest, Graph, Path),
    global(Graph, Path, Colour).
get_paths(Src, Dest, Graph, until(Colour1, Colour2), Path) :-
    get_paths(Src, Dest, Graph, Path),
    until(Graph, Path, Colour1, Colour2).
get_paths(Src, Dest, Graph, next(Formula), [H1|[H2|Path]]) :-
    get_paths(Src, Dest, Graph, [H1|[H2|Path]]),
    get_paths(H2, Dest, Graph, Formula, [H2|Path]).
get_paths(Src, Dest, Graph, and(Formula1, Formula2), Path) :-
    get_paths(Src, Dest, Graph, Formula1, Path),
    get_paths(Src, Dest, Graph, Formula2, Path).
get_paths(Src, Dest, Graph, or(Formula1, Formula2), Path) :-
    get_paths(Src, Dest, Graph, Formula1, Path);
    get_paths(Src, Dest, Graph, Formula2, Path).
get_paths(Src, Dest, Graph, not(Formula), Path) :-
    not(get_paths(Src, Dest, Graph, Formula, Path)).
get_paths(Src, Dest, [V, E], Colour, Path) :-
    get_paths(Src, Dest, [V, E], Path),
    get_colour(V, Src, Colour).

:- use_module(library(aggregate)).
% Find shortest path
getPath(Src, Dest, Graph, Formula, ShortestPath) :-
    aggregate(min(Length, Path),
              (get_paths(Src, Dest, Graph, Formula, Path), length(Path, Length)),
              min(Length, ShortestPath)).