%% =============================================================================
%% == Macros
%% =============================================================================
-define(FIELD, load1).

%% =============================================================================
%% == Records
%% =============================================================================
-record(loadavg, {load1, load5, load15, runnable, processes}).

