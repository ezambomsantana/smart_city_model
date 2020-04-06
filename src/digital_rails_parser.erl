-module(digital_rails_parser).
-include_lib("xmerl/include/xmerl.hrl").

-export([show/1]).

show(ok) -> 
  [];
show(Filename) -> 
  HookFun = fun
    (#xmlElement{name=Name, attributes=[], content=Cont}, GS) -> 
      {{Name, Cont}, GS}; 
    (#xmlElement{name=Name, attributes=Attributes, content=Cont}, GS) -> 
      {{Name, lists:map(
        fun(#xmlAttribute{name=AttribName, value=AttribValue}) -> 
          {AttribName, AttribValue} end, Attributes), Cont}, GS}; 
    (E, GS) -> {E, GS} 
  end,

  AccFun = fun
    (#xmlText{value=V} = _E, Acc, GS) -> 
      case re:run(V, "^\\s*$") of
        {match, _} -> {Acc, GS}; 
        nomatch -> {[V | Acc], GS} 
      end; 
    (_E,Acc,GS) -> {[_E|Acc], GS} 
  end,

  {{'digital-rails', Signals}, _} = xmerl_scan:file(Filename, [{hook_fun, HookFun}, {acc_fun, AccFun}]),
  Signals.