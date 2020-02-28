-module(map_parser_test).

-export([show_tests/0]).


%%%%%%%%%%%%%%%%%
% TESTES %%%%%%%%
%%%%%%%%%%%%%%%%%

show_tests() ->
    io:format("~n~nExecutando testes~n~n"),
    io:format("----------------------~n"),
    Infilename = "small-network.xml",
    map_parser:show(Infilename, false),
    io:format("~nTestes executados (se não deu pau, então tá bom) =)~n").




