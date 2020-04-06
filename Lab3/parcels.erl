%%%-------------------------------------------------------------------
%%% @author Grzegorz Poręba
%%% @copyright (C) 2020, <agh.cs.wokstym>
%%% @doc
%%%
%%% @end
%%% Created : 29. mar 2020 12:29
%%%-------------------------------------------------------------------
-module(parcels).
-author("Grzegorz Poręba").

%% API
-export([prepData/2, findMyParcelLocker/2, spawnProcessForEachPerson/2, resultReceiver/3, calculateAndSendResults/2, calculateAndSendResultsDivided/2, splitWork/3, test/2]).


test(PeopleNr, LockersNr) ->
  io:format("People: ~p, Lockers: ~p~n",[PeopleNr, LockersNr]),
  {People, Lockers} = prepData(PeopleNr, LockersNr),
  testSeq(People, Lockers),
  testEachProcess(People, Lockers),
  testPartProcess(People, Lockers, 4).


% ================== Sequential test ==================
testSeq(People, Lockers) ->
  Start = now(),
  Results = [{Person, findMyParcelLocker(Person, Lockers)} || Person <- People],
  End = now(),
  io:format("Sequential took ~p seconds~n", [timer:now_diff(End, Start) / 1000000]).


% ================== For each parallel test ==================
testEachProcess(People, Lockers) ->
  Start = now(),
  MyPID = self(),
  register(receiverPID, spawn(parcels, resultReceiver, [[], length(People), MyPID])),
  spawnProcessForEachPerson(People, Lockers),
  receive
    Results ->
      End = now(),
      io:format("Each one took ~p seconds~n", [timer:now_diff(End, Start) / 1000000])
  end.


spawnProcessForEachPerson([], _) -> ok;
spawnProcessForEachPerson([PeopleH | PeopleT], Lockers) ->
  spawn(parcels, calculateAndSendResults, [PeopleH, Lockers]),
  spawnProcessForEachPerson(PeopleT, Lockers).



calculateAndSendResults(PersonLocation, [LockerLocationsH | LockerLocationsT]) ->
  Res = findMyParcelLocker(PersonLocation, LockerLocationsT, LockerLocationsH),
  receiverPID ! {PersonLocation, Res}.


% ================== Divided parallel test, 4 cores ==================
testPartProcess(People, Lockers, Cores) ->
  Start = now(),
  MyPID = self(),
  register(receiverPID, spawn(parcels, resultReceiver, [[], length(People), MyPID])),
  splitWork(People, Lockers, Cores),
  receive
    Results ->
      End = now(),
      io:format("Devided took ~p seconds~n", [timer:now_diff(End, Start) / 1000000])
  end.


splitWork([], _, 0) -> ok;
splitWork(People, Lockers, Divider) ->
  {Chunk, Rest} = lists:split(length(People) div Divider, People),
  spawn(parcels, calculateAndSendResultsDivided, [Chunk, Lockers]),
  splitWork(Rest, Lockers, Divider - 1).


calculateAndSendResultsDivided(People, Lockers) ->
  Res = [{Person, findMyParcelLocker(Person, Lockers)} || Person <- People],
  receiverPID ! Res.


% ================== Helper Functions ==================

% Helper function for parallel calculations to accumulate results
resultReceiver(ResultList, 0, ParentPID) -> ParentPID ! lists:flatten(ResultList);
resultReceiver(ResultList, LeftResNr, ParentPID) ->
  receive
    stop -> ok;
    Result ->
      case is_list(Result) of
        true -> resultReceiver([Result | ResultList], LeftResNr - length(Result), ParentPID);
        false -> resultReceiver([Result | ResultList], LeftResNr - 1, ParentPID)
      end
  end.


%% Generate People and Lockers locations
prepData(PeopleNr, LockersNr) ->
  People = [{rand:uniform(1000), rand:uniform(1000)} || _ <- lists:seq(1, PeopleNr)],
  Lockers = [{rand:uniform(1000), rand:uniform(1000)} || _ <- lists:seq(1, LockersNr)],
  {People, Lockers}.


countDist({PersonX, PersonY}, {LockerX, LockerY}) ->
  math:sqrt(math:pow(PersonX - LockerX, 2) + math:pow(PersonY - LockerY, 2)).


findMyParcelLocker(PersonLocation, [LockerLocationsH | LockerLocationsT]) ->
  findMyParcelLocker(PersonLocation, LockerLocationsT, LockerLocationsH).
findMyParcelLocker(_, [], ClosestLocker) -> ClosestLocker;
findMyParcelLocker(PersonLocation, [LockerLocationsH | LockerLocationsT], ClosestLocker) ->
  case countDist(PersonLocation, LockerLocationsH) < countDist(PersonLocation, ClosestLocker) of
    true -> findMyParcelLocker(PersonLocation, LockerLocationsT, LockerLocationsH);
    _ -> findMyParcelLocker(PersonLocation, LockerLocationsT, ClosestLocker)
  end.