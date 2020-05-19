defmodule PollutionDataStream do


  def identifyStations(list)do
    list
    |> Enum.uniq_by(
         fn (%{:location => coords}) ->
           coords
         end
       )
  end

  def parseOneLine(line)do
    [date, time, fCoord, sCoord, val] = String.split(line, ",")

    date = date
           |> String.split("-")
           |> Enum.reverse
           |> Enum.map(
                fn (sVal) ->
                  Integer.parse(sVal)
                  |> case do {val, _} ->
                    val end
                end
              )
           |> :erlang.list_to_tuple

    time = time
           |> String.split(":")
           |> Enum.map(
                fn (sVal) ->
                  Integer.parse(sVal)
                  |> case do {val, _} ->
                    val end
                end
              )
           |> Enum.concat([0])
           |> :erlang.list_to_tuple

    {fCoord, _} = Float.parse(fCoord)
    {sCoord, _} = Float.parse(sCoord)
    {val, _} = Integer.parse(val)

    %{:datetime => {date, time}, :location => {fCoord, sCoord}, :pollutionLevel => val}
  end

  def importLinesFromCSV() do
    Path.expand('./res/pollution.csv')
    |> File.stream!
    |> Stream.map(&parseOneLine/1)
    |> Enum.to_list
  end

  def loadData()do
    :pollution_sup.start_link()

    exec_station =
      fn () ->
        importLinesFromCSV()
        |> PollutionData.identifyStations
        |> Enum.map(fn (station) -> station[:location]  end)
        |> Enum.each(
             fn (coords) ->
               :pollution_gen_server.addStation(
                 "station_#{elem(coords, 0)}_#{elem(coords, 1)}",
                 coords
               )
             end
           )
      end

    exec_values =
      fn () ->
        importLinesFromCSV()
        |> Enum.each(
             fn (station) ->
               :pollution_gen_server.addValue(
                 station[:location],
                 'PM10',
                 station[:datetime],
                 station[:pollutionLevel]
               )
             end
           )
      end

    loadStationsTime = exec_station
                       |> :timer.tc([])
                       |> elem(0)

    loadValuesTime = exec_values
                     |> :timer.tc([])
                     |> elem(0)

    IO.puts("Loading stations time was #{loadStationsTime / 1000000} and values #{loadValuesTime / 1000000}")
  end

  def test2Func() do
    exec_mean = fn (coords, val) ->
      :pollution_gen_server.getStationMean(coords, val)
    end

    {time, value} = exec_mean
                    |> :timer.tc([{20.067, 49.984}, 'PM10'])
    IO.puts("#{time}, #{value}")



    exec_daily = fn (coords, date) ->
      :pollution_gen_server.getDailyMean(coords, date)
    end

    {time, value} = exec_daily
                    |> :timer.tc([{20.067, 49.984}, {{2017, 5, 3}, 1}])
    IO.puts("#{time}, #{value}")
  end
end
