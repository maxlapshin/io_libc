-module(io_libc).

-export([fwrite/2, format/2]).

-on_load(init_nif/0).

init_nif() ->
  NifDir = case code:priv_dir(io_libc) of
    {error, _} ->
      SelfPath = code:which(?MODULE),
      EbinDir = filename:dirname(SelfPath),
      PacketDir = filename:dirname(EbinDir),
      filename:join(PacketDir, priv);
    PrivDir ->
      PrivDir
  end,

  NifPath = filename:join(NifDir, io_libc),
  erlang:load_nif(NifPath, 0).


format(Format, Data) ->
  fwrite(Format, Data).

fwrite(_Format, _Data) ->
  erlang:error({io_libc, nif_not_loaded}).
