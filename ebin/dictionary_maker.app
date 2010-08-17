%%-*- mode: erlang -*-
{application, dictionary_maker,
 [
  {description, "dictionary_maker"},
  {vsn, "1"},
  {modules, [
             dictionary_maker,
             dictionary_maker_app,
             dictionary_maker_sup,
             dictionary_maker_resource
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { dictionary_maker_app, []}},
  {env, []}
 ]}.
