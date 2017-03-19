-module(band_supervisor).
-compile(export_all).
-behavior(supervisor).

start_link(Type) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

init(jamband) ->
  {ok, {{simple_one_for_one, 3, 60},
        [{jam_musician,
          {musicians, start_link, []},
          temporary, 1000, worker, [musicians]}
        ]}};
init(lenient) ->
  init({one_for_one, 3, 60});
init(angry) ->
  init({rest_for_one, 2, 60});
init(jerk) ->
  init({one_for_all, 1, 60});
init({RestartStrategy, MaxRestart, MaxTime}) ->
  {ok, {{RestartStrategy, MaxRestart, MaxTime},
        [{singer,
          {musicians, start_link, [singer, good]},
          permanent, 1000, worker, [musicians]},
         {bass,
          {musicians, start_link, [bass, good]},
          temporary, 1000, worker, [musicians]},
         {drums,
          {musicians, start_link, [drums, bad]},
          transient, 1000, worker, [musicians]},
         {keytar,
          {musicians, start_link, [keytar, good]},
          transient, 1000, worker, [musicians]}
        ]}}.



