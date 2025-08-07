% module for the key

-module(key).
-compile(export_all).

% a random number between 1 and 1,000,000,000 for the key
generate() ->
  random:uniform(1_000_000_000).

% check if Key is in between From and To or equal to To
between(Key, From, To) ->
    if
      % (From, To]
    	(From < To) and (Key > From) and (Key =< To) ->
    	    true;
      % Key <= To < From < Key
    	(From > To) and ((Key > From) or (Key =< To)) ->
    	    true;
    	From == To ->
    	    true;
    	true ->
    	    false
    end.