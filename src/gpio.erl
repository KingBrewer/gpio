%% @author Paolo Oliveira <paolo@fisica.ufc.br>.
%% @copyright 2015-2016 Paolo Oliveira (license MIT)
%% @version 1.1.0
%% @doc
%% A simple, pure erlang implementation of a module for <b>Raspberry Pi's General Purpose
%% Input/Output</b> (GPIO), using the standard Linux kernel interface for user-space, sysfs,
%% available at <b>/sys/class/gpio/</b>.
%% It has been slightly updated by KingBrewer, retaining the original
%% interface for backward compatibility.
%% It is now an OTP application with clearly defined supervision tree.
%% @end

-module(gpio).
-export([start/0, init/1, init/2, init/3, read/1, write/2, stop/1, stop/0]).
-author('Paolo Oliveira <paolo@fisica.ufc.br>').

%% API

% @doc: Starts the application (if not already started if part of a release)
start() ->
    application:start(gpio).

% @doc: Initialize a Pin as input or output with custom active_low setting.
init(Pin, Direction, ActiveLow) ->
    gpio_srv:start(Pin, Direction, ActiveLow).

% @doc: Initialize a Pin as input or output.
init(Pin, Direction) ->
  init(Pin, Direction, undefined).

% @doc: A shortcut to initialize a Pin as output.
init(Pin) ->
  init(Pin, out).

% @doc: Stop using and release the Pin
stop(Pin) ->
  gpio_srv:stop(Pin).

% @doc: Read from an initialized Pin
read(Pin) ->
  gpio_srv:read(Pin).

% @doc: Write value Val to an initialized Pin
write(Pin, Val) ->
  gpio_srv:write(Pin, Val).

% @doc: Stops the application, closing all open pins and cleaning up the mess.
stop() ->
  application:stop(gpio).


%% End of Module.
