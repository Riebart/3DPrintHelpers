(* Untis are irrelevant, but use the same for all of them *)

residualMeasurements={
    {20, 20, 0.25}, {80, 20, 0.19}, {140, 20, 00.165}, {200, 20, 0.175},
    {20, 80, -0.035}, {80, 80, -0.005}, {140, 80, -0.05}, {200, 80, -0.075},
    {20, 140, -0.095}, {80, 140, -0.095}, {140, 140, -0.095}, {200, 140, -0.1},
    {20, 200, -0.07}, {80, 200, -0.095}, {140, 200, -0.1}, {200, 200, -0.1}
}

Off[InterpolatingFunction::dmval]
residualInt = Interpolation[{#[[{1,2}]],#[[3]]}&/@residualMeasurements]

centrePoint = {110, 110, -0.05}
paperThickness = 0.05
temperatures = <| Nozzle -> 200, Bed -> 60 |>

bedMeshConfigBlockLines = "#*# [bed_mesh 20231226-11x3-hot60c]
#*# version = 1
#*# points =
#*#       -0.065000, -0.050000, -0.015000, -0.060000, -0.057500, -0.005000, -0.035000, -0.040000, 0.022500, -0.010000, -0.012500
#*#       -0.115000, -0.060000, -0.030000, -0.082500, -0.087500, -0.022500, -0.040000, -0.087500, -0.035000, -0.032500, -0.087500
#*#       -0.057500, -0.067500, -0.042500, -0.087500, -0.080000, -0.032500, -0.072500, -0.100000, -0.052500, -0.062500, -0.082500
#*#       -0.027500, -0.002500, 0.032500, -0.032500, -0.040000, 0.025000, -0.012500, -0.050000, -0.002500, -0.010000, -0.055000
#*#       -0.025000, -0.032500, -0.012500, -0.057500, -0.060000, -0.020000, -0.042500, -0.067500, -0.030000, -0.025000, -0.035000
#*#       -0.042500, -0.005000, 0.035000, -0.032500, -0.047500, 0.027500, -0.012500, -0.037500, -0.012500, 0.020000, -0.037500
#*#       -0.035000, -0.035000, -0.017500, -0.065000, -0.082500, -0.027500, -0.047500, -0.092500, -0.045000, -0.042500, -0.047500
#*#       -0.045000, -0.007500, 0.042500, -0.045000, -0.057500, -0.010000, -0.037500, -0.060000, -0.025000, -0.022500, -0.057500
#*#       -0.052500, -0.070000, -0.030000, -0.087500, -0.075000, -0.025000, -0.050000, -0.095000, -0.047500, -0.042500, -0.032500
#*#       -0.060000, -0.045000, 0.007500, -0.057500, -0.062500, 0.002500, -0.025000, -0.060000, -0.035000, -0.007500, -0.030000
#*#       -0.097500, -0.115000, -0.067500, -0.122500, -0.120000, -0.062500, -0.070000, -0.125000, -0.080000, -0.072500, -0.090000
#*# x_count = 11
#*# y_count = 11
#*# mesh_x_pps = 5
#*# mesh_y_pps = 5
#*# algo = bicubic
#*# tension = 0.2
#*# min_x = 15.0
#*# max_x = 200.0
#*# min_y = 15.0
#*# max_y = 183.9
"//StringSplit[#, "\n"]&//StringTrim/@#&

GetProperty = Function[{varName, bmcl},
    (StringSplit[#," "]&/@bmcl)//
        Select[#[[2]]==varName&]//#[[1,4]]&//ToExpression
]

bedMeshProperties = <|
    x -> <|
        count -> GetProperty["x_count", bedMeshConfigBlockLines],
        min -> GetProperty["min_x", bedMeshConfigBlockLines],
        max -> GetProperty["max_x", bedMeshConfigBlockLines]
    |>,
    y -> <|
        count -> GetProperty["y_count", bedMeshConfigBlockLines],
        min -> GetProperty["min_y", bedMeshConfigBlockLines],
        max -> GetProperty["max_y", bedMeshConfigBlockLines]
    |>
|>

bedMeshProperties[x][dx] = (
    bedMeshProperties[x][max]-bedMeshProperties[x][min])/(
        bedMeshProperties[x][count]-1)

bedMeshProperties[y][dy] = (
    bedMeshProperties[y][max]-bedMeshProperties[y][min])/(
        bedMeshProperties[y][count]-1)


bedMeshStart = Position[StringMatchQ[bedMeshConfigBlockLines,__~~"points"~~__], True][[1,1]]
bedMeshHeights = (
    StringTake[#,4;;]//StringSplit[#,","]&//ToExpression
)&/@(
    bedMeshConfigBlockLines[[
        bedMeshStart+1;;
        bedMeshStart+bedMeshProperties[y][count]
    ]])

bedMeshCoordinates = Table[{x, y},
    {y,bedMeshProperties[y][min], bedMeshProperties[y][max], bedMeshProperties[y][dy]},
    {x,bedMeshProperties[x][min], bedMeshProperties[x][max], bedMeshProperties[x][dx]}
    ]

newBedMesh = bedMeshHeights + Map[residualInt@@#&, bedMeshCoordinates, {2}] + paperThickness

newConfigPoints = Join[
    {"points ="},
    "     "<>#&/@StringSplit[ExportString[newBedMesh, "CSV"],"\n"]
]

configBlockWithoutPoints = bedMeshConfigBlockLines//Join[
    #[[;;bedMeshStart]],
    #[[bedMeshStart+bedMeshProperties[y][count]+1;;]]
]&

newBedMeshBlockLines = Join[configBlockWithoutPoints,"#*# "<>#&/@newConfigPoints]

Echo[newBedMeshBlockLines//StringRiffle[#,"\n"]&]
