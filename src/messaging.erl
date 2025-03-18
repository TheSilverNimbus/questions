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
-export([apps/0, run_app/4, launch_app/2, interact/2, show_state/1, show_history/1]).

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
    }
  ].

survey_app(AppName, [], App, History) ->
  run_app(AppName, App, intro, History);
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

run_app(survey_app = AppName, App, StateId, History) ->
  Q = lists:keyfind(StateId, #question.id, App),
  Qs = anchor_qs(Q#question.anchor, App),
  survey_app(AppName, Qs, App, History);
run_app(AppName, App, StateId, History) ->
  State = proplists:get_value(StateId, App),
  OutMsg = State#app_state.msg,
  io:format("~p > ~s~n", [AppName, OutMsg]),

  receive
    print_state ->
      io:format("~p is at ~p~n", [AppName, StateId]),
      run_app(AppName, App, StateId, History);  % loop back to the same state

    print_history ->
      io:format("~p~n", [History]),
      run_app(AppName, App, StateId, History);  % loop back to the same state

    InMsg ->
      Logic = State#app_state.logic,
      NextStateId = Logic(InMsg),
      NewHistory = [{StateId, InMsg}|History], % update the history
      run_app(AppName, App, NextStateId, NewHistory)
  end.

launch_app(AppName, InitStateId) ->
  Pid =
    spawn(
      messaging,
      run_app,
      [
        AppName,
        proplists:get_value(AppName, messaging:apps()),
        InitStateId,
        []
      ]
    ),
  register(AppName, Pid),
  ok.

interact(AppName, Msg) ->
  AppName ! Msg,
  ok.

show_state(AppName) ->
  case whereis(AppName) of
    undefined -> io:format("Application ~p is not running~n", [AppName]);
    _Pid -> AppName ! print_state
  end,
  ok.

show_history(AppName) ->
  case whereis(AppName) of
    undefined -> io:format("Application ~p is not running~n", [AppName]);
    _Pid -> AppName ! print_history
  end,
  ok.
