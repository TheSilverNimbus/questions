%%%-------------------------------------------------------------------
%%% @author TheSilverNimbus
%%% @copyright (C) 2025, Precision Development (PxD)
%%% @doc
%%% TODO
%%% @end
%%%-------------------------------------------------------------------
-module(messaging).

-author("TheSilverNimbus").

%% API
-export([apps/0, run_app/5, launch_app/2, interact/3, show_state/2, show_history/2]).

-record(app_state, {msg, logic}).

-record(question, {id, msg, opts = [], anchor = []}).

apps() ->
  [
    {
      example_app,
      [
        {
          intro,
          #app_state{
            msg = "Choose a topic: (1) Rice (2) Cotton",
            logic =
            fun(Resp) ->
              case Resp of
                "1" -> rice1;
                "2" -> cotton1;
                _Otherwise -> intro
              end
            end
          }
        },
        {
          rice1,
          #app_state{
            msg =
            "Rice is the seed of the grass species Oryza glaberrima or Oryza "
            "sativa.",
            logic = fun(_) -> rice2 end
          }
        },
        {
          rice2,
          #app_state{
            msg =
            "As a cereal grain, it is the most widely consumed staple food for "
            "a large part of the world's human population, especially in Asia "
            "and Africa.",
            logic = fun(_) -> rice3 end
          }
        },
        {
          rice3,
          #app_state{
            msg =
            "It is the agricultural commodity with the third-highest worldwide "
            "production, after sugarcane and maize.",
            logic = fun(_) -> intro end
          }
        },
        {
          cotton1,
          #app_state{
            msg =
            "Cotton is a soft, fluffy staple fiber that grows in a boll, or "
            "protective case, around the seeds of the cotton plants of the "
            "genus Gossypium in the mallow family Malvaceae.",
            logic = fun(_) -> cotton2 end
          }
        },
        {
          cotton2,
          #app_state{
            msg =
            "The fiber is almost pure cellulose. Under natural conditions, the "
            "cotton bolls will increase the dispersal of the seeds.",
            logic = fun(_) -> intro end
          }
        }
      ]
    },
    {
      survey_app,
      [
        #question{
          id = intro,
          msg = "Choose a topic: (1) Rice (2) Cotton",
          opts = ["1", "2"]
        },
        #question{
          id = rice1,
          msg =
          "Rice is the seed of the grass species Oryza glaberrima or Oryza "
          "sativa. Choose a topic: (2) Rice2 (3) Rice3",
          opts = ["2", "3"],
          anchor = [{intro, "1"}]
        },
        #question{
          id = rice2,
          msg =
          "As a cereal grain, it is the most widely consumed staple food for a "
          "large part of the world's human population, especially in Asia and "
          "Africa.",
          anchor = [{rice1, "2"}, {intro, "1"}]
        },
        #question{
          id = rice3,
          msg =
          "It is the agricultural commodity with the third-highest worldwide "
          "production, after sugarcane and maize.",
          anchor = [{rice1, "3"}, {intro, "1"}]
        },
        #question{
          id = cotton1,
          msg =
          "Cotton is a soft, fluffy staple fiber that grows in a boll, or "
          "protective case, around the seeds of the cotton plants of the genus "
          "Gossypium in the mallow family Malvaceae.",
          anchor = [{intro, "2"}]
        },
        #question{
          id = cotton2,
          msg =
          "The fiber is almost pure cellulose. Under natural conditions, the "
          "cotton bolls will increase the dispersal of the seeds.",
          anchor = [{intro, "2"}]
        }
      ]
    },
    {
      adventure_game,
      [
        {
          intro,
          #app_state{
            msg =
            "You stand at the entrance of a dark cave. Do you (1) Enter or (2) "
            "Run away?",
            logic =
            fun(Resp) ->
              case Resp of
                "1" -> cave_entry;
                "2" -> coward_ending;
                _Otherwise -> intro
              end
            end
          }
        },
        {
          cave_entry,
          #app_state{
            msg =
            "Inside the cave, you find two paths. Do you go (1) Left or (2) "
            "Right?",
            logic =
            fun(Resp) ->
              case Resp of
                "1" -> left_path;
                "2" -> right_path;
                _Otherwise -> cave_entry
              end
            end
          }
        },
        {
          left_path,
          #app_state{
            msg =
            "You see a treasure chest guarded by a sleeping dragon. Do you (1) "
            "Fight or (2) Sneak?",
            logic =
            fun(Resp) ->
              case Resp of
                "1" -> fight_dragon;
                "2" -> sneak_past;
                _Otherwise -> left_path
              end
            end
          }
        },
        {
          fight_dragon,
          #app_state{
            msg =
            "You bravely fight the dragon! It's a tough battle... but you win! "
            "You take the treasure and leave the cave victorious!",
            logic = fun(_) -> intro end
          }
        },
        {
          sneak_past,
          #app_state{
            msg = "You try to sneak past the dragon... but it wakes up! You "
            "run for your life, barely escaping. You survive but without the "
            "treasure.",
            logic = fun(_) -> intro end
          }
        },
        {
          right_path,
          #app_state{
            msg =
            "You step on a trap! Do you (1) Try to disarm it or (2) Retreat "
            "carefully?",
            logic =
            fun(Resp) ->
              case Resp of
                "1" -> disarm_trap;
                "2" -> retreat;
                _Otherwise -> right_path
              end
            end
          }
        },
        {
          disarm_trap,
          #app_state{
            msg =
            "You carefully disarm the trap. Success! You find a hidden exit "
            "and escape safely with some small treasure.",
            logic = fun(_) -> intro end
          }
        },
        {
          retreat,
          #app_state{
            msg =
            "You carefully retreat and return to the cave entrance. Maybe "
            "another day...",
            logic = fun(_) -> intro end
          }
        },
        {
          coward_ending,
          #app_state{
            msg =
            "You run away, never knowing what treasures (or dangers) lay "
            "within the cave...",
            logic = fun(_) -> intro end
          }
        }
      ]
    }
  ].

survey_app(AppName, [], App, History) ->
  run_app(AppName, App, intro, History, 0);
survey_app(AppName, [Q|Qs] = Questions, App, History) ->
  Qid = Q#question.id,
  Msg = Q#question.msg,
  Opts = Q#question.opts,
  Anchor = Q#question.anchor,
  io:format("~p > ~s~n", [AppName, Msg]),

  receive
    print_state ->
      io:format("~p is at ~p~n", [AppName, Qid]);

    print_history ->
      io:format("~p~n", [History]);

    InMsg ->
      case Opts of
        [] ->
          survey_app(AppName, Qs, App, update_history({Qid, InMsg}, History));

        Opts ->
          case lists:member(InMsg, Opts) of
            true ->
              AnchorQs = anchor_qs([{Qid, InMsg}|Anchor], App),

              survey_app(
                AppName,
                AnchorQs ++ Qs,
                App,
                update_history({Qid, InMsg}, History)
              );

            false ->
              survey_app(AppName, Questions, App, History)
          end
      end
  end.

anchor_qs(Anchor, App) ->
  [Q || Q <- App, Q#question.anchor =:= Anchor].

update_history({Qid, _Ans} = H, History) ->
  lists:keystore(Qid, 1, History, H).

run_app(survey_app = AppName, App, StateId, History, _SessionId) ->
  Q = lists:keyfind(StateId, #question.id, App),
  Qs = anchor_qs(Q#question.anchor, App),
  survey_app(AppName, Qs, App, History);
run_app(AppName, App, StateId, History, SessionId) ->
  % Store the app and history in the "Process Dictionary":
  put(app, App),
  put(history, History),
  put(session_id, SessionId),

  State = proplists:get_value(StateId, App),
  OutMsg = State#app_state.msg,

  io:format("~p > ~s~n", [AppName, OutMsg]),

  receive
    {interact, SessionIdFromMsg, InMsg} ->
      case SessionIdFromMsg of
        SessionId ->
          Logic = State#app_state.logic,
          NextStateId = Logic(InMsg),
          NewHistory = [{StateId, InMsg}|History],
          save_state(AppName, SessionId, NextStateId, NewHistory),
          run_app(AppName, App, NextStateId, NewHistory, SessionId);
        _ ->
          io:format("Invalid session ID: `~p`~n", [SessionIdFromMsg]),
          run_app(AppName, App, StateId, History, SessionId)
      end;

    print_state ->
      io:format("~p is at ~p~n", [AppName, StateId]),
      run_app(AppName, App, StateId, History, SessionId);

    print_history ->
      print_full_history(AppName),
      run_app(AppName, App, StateId, History, SessionId)

  after 120000 ->
    %% 120 second timeout
    io:format(
      "Session `~p` for app `~p` timed out due to inactivity~n",
      [SessionId, AppName]
    ),

    ProcessName = make_process_name(AppName, SessionId),

    unregister(ProcessName),

    {error, session_timed_out}
  end.

launch_app(AppName, InitStateId) ->
  case proplists:is_defined(AppName, messaging:apps()) of
    false ->
      io:format("Application `~p` is not defined~n", [AppName]),
      {error, undefined_app};

    true ->
      SessionId = make_session_id(),

      {SavedStateId, SavedHistory} = load_state(AppName, SessionId),

      {StateId, History} =
        case SavedStateId of
          undefined -> {InitStateId, []};
          _ -> {SavedStateId, SavedHistory}
        end,

      Pid =
        spawn(
          messaging,
          run_app,
          [
            AppName,
            proplists:get_value(AppName, messaging:apps()),
            StateId,
            History,
            SessionId
          ]
        ),

      ProcessName = make_process_name(AppName, SessionId),

      case whereis(ProcessName) of
        undefined ->
          register(ProcessName, Pid),
          {ok, SessionId};

        _Pid ->
          io:format("A session for app `~p` is already loaded~n", [AppName]),
          {error, app_already_loaded}
      end
  end.

interact(AppName, SessionId, Msg) ->
  ProcessName = make_process_name(AppName, SessionId),

  case whereis(ProcessName) of
    undefined ->
      io:format("Session `~p` for app `~p` not found~n", [SessionId, AppName]),
      {error, session_not_found};

    _Pid ->
      ProcessName ! {interact, SessionId, Msg},
      ok
  end.

show_state(AppName, SessionId) ->
  ProcessName = make_process_name(AppName, SessionId),

  case whereis(ProcessName) of
    undefined ->
      io:format("Session `~p` for app `~p` not found~n", [SessionId, AppName]),
      {error, session_not_found};

    _Pid ->
      ProcessName ! print_state,
      ok
  end.

show_history(AppName, SessionId) ->
  ProcessName = make_process_name(AppName, SessionId),

  case whereis(ProcessName) of
    undefined ->
      io:format("Session `~p` for app `~p` not found~n", [SessionId, AppName]),
      {error, session_not_found};

    _Pid ->
      ProcessName ! print_history,
      ok
  end.

print_full_history(AppName) ->
  History = get(history),
  App = get(app),

  lists:map(
    fun({StateId, UserInput}) ->
      State = proplists:get_value(StateId, App),

      % Handle `app_state` and `question` types:
      Msg =
        case State of
          #app_state{} -> State#app_state.msg;
          #question{} -> State#question.msg;
          _ -> "Unknown state"
        end,

      io:format(
        "~p > (app prompt) ~s~n~s > (user input) ~s~n",
        [AppName, Msg, AppName, UserInput]
      )
    end,
    lists:reverse(History)
  ),

  ok.

filename(AppName, SessionId) ->
  "state_" ++ atom_to_list(AppName) ++ "_" ++ SessionId ++ ".dat".

save_state(AppName, SessionId, StateId, History) ->
  FileName = filename(AppName, SessionId),
  StateData = {StateId, History},
  Bin = term_to_binary(StateData),
  file:write_file(FileName, Bin).

load_state(AppName, SessionId) ->
  FileName = filename(AppName, SessionId),

  case file:read_file(FileName) of
    {ok, Bin} -> binary_to_term(Bin);
    {error, _} -> {undefined, []}  %% No saved state, start fresh
  end.

make_session_id() ->
  UUID = crypto:strong_rand_bytes(16),
  lists:flatten([io_lib:format("~2.16.0B", [N]) || <<N>> <= UUID]).

make_process_name(AppName, SessionId) ->
  list_to_atom(atom_to_list(AppName) ++ "_" ++ SessionId).
