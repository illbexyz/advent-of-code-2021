-module(day_03).
-export([main/0]).

get_all_lines(Device) ->
    case io:get_line(Device, "") of
      eof  -> [];
      Line -> Line ++ get_all_lines(Device)
    end.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_column(Lines, ColIndex) -> 
  lists:map(
    fun (Line) ->
      string:slice(Line, ColIndex, 1)
    end,
  Lines).

bin_to_int(Binary) ->
  LastIdx = length(Binary) - 1,
  BinWithIndexes = lists:zip(lists:seq(0, LastIdx), Binary),
  Values = lists:map(
    fun ({ Idx, Bit }) ->
      trunc(list_to_integer(Bit) * math:pow(2, (LastIdx - Idx)))
    end,
    BinWithIndexes
  ),
  lists:sum(Values).

gamma_bit(Column) ->
  Ones = lists:filter(fun (Num) -> Num == "1" end, Column),
  case string:length(Ones) > string:length(Column) / 2 of
    true -> "1";
    false -> "0"
  end.

gamma(Lines) ->
  ColIndexes = lists:seq(0, length(lists:nth(1, Lines)) - 1),
  lists:map(fun (ColIdx) -> gamma_bit(get_column(Lines, ColIdx)) end, ColIndexes).

epsilon_from_gamma(Gamma) ->
  lists:map(
    fun (Bit) ->
      case Bit of 
        "0" -> "1";
        "1" -> "0"
      end
    end,
    Gamma
  ).

part_one(Lines) ->
  Gamma = gamma(Lines),
  Epsilon = epsilon_from_gamma(Gamma),
  bin_to_int(Gamma) * bin_to_int(Epsilon).

main() -> 
  Lines = string:split(readlines("input.txt"), "\n", all),
  PartOne = part_one(Lines),
  io:format("Part One: ~p\n", [PartOne]).