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

invert_bit("1") -> "0";
invert_bit("0") -> "1".

get_column(Lines, ColIndex) -> 
  lists:map(
    fun (Line) -> string:slice(Line, ColIndex, 1) end,
    Lines
  ).

bin_to_int(Binary) ->
  LastIdx = string:len(Binary) - 1,
  BinWithIndexes = lists:zip(lists:seq(0, LastIdx), Binary),
  Values = lists:map(
    fun ({ Idx, Bit }) ->
      trunc(list_to_integer([Bit]) * math:pow(2, (LastIdx - Idx)))
    end,
    BinWithIndexes
  ),
  lists:sum(Values).

most_common_bit(Column) ->
  Ones = lists:filter(fun (Num) -> Num == "1" end, Column),
  case string:length(Ones) >= (string:length(Column) / 2) of
    false -> "0";
    true -> "1"
  end.

least_common_bit(Column) -> invert_bit(most_common_bit(Column)).

gamma(Lines) ->
  ColIndexes = lists:seq(0, length(lists:nth(1, Lines)) - 1),
  lists:flatmap(
    fun (ColIdx) -> most_common_bit(get_column(Lines, ColIdx)) end,
    ColIndexes
  ).

epsilon_from_gamma(Gamma) ->
  lists:flatmap(fun (Bit) -> invert_bit([Bit]) end, Gamma).

iter_p2([Line], _Fn, _ColIdx) -> Line;
iter_p2(Lines, CommonBitFn, ColIdx) ->
  CommonBit = CommonBitFn(get_column(Lines, ColIdx)),
  RemainingLines = lists:filter(
    fun (Line) -> string:slice(Line, ColIdx, 1) == CommonBit end,
    Lines
  ),
  iter_p2(RemainingLines, CommonBitFn, ColIdx + 1).

part_one(Lines) ->
  Gamma = gamma(Lines),
  Epsilon = epsilon_from_gamma(Gamma),
  bin_to_int(Gamma) * bin_to_int(Epsilon).

part_two(Lines) ->
  Oxygen = iter_p2(Lines, fun most_common_bit/1, 0),
  CO2 = iter_p2(Lines, fun least_common_bit/1, 0),
  bin_to_int(Oxygen) * bin_to_int(CO2).

main() -> 
  Lines = string:split(readlines("input.txt"), "\n", all),
  PartOne = part_one(Lines),
  io:format("Part One: ~p\n", [PartOne]),
  PartTwo = part_two(Lines),
  io:format("Part Two: ~p\n", [PartTwo]).