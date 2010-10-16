role :lingserver, "ling-staging-dictionary-maker"
role :development, "ling-yogurt-dictionary-maker"

task :init, :roles => :lingserver do
  run "cd /home/dictionary_maker && /usr/bin/hg clone /hg/dictionary_maker && cd /home/dictionary_maker/dictionary_maker && ./rebar get-deps && ./rebar compile"
end

task :deploy, :roles => :lingserver do
  run "cd /home/dictionary_maker/dictionary_maker && /usr/bin/hg pull -u && ./rebar compile"
end

task :develinit, :roles => :development do
  run "cd /home/dictionary_maker && /usr/bin/hg clone ssh://staging.ling.wisc.edu//hg/dictionary_maker && cd /home/dictionary_maker/dictionary_maker && ./rebar get-deps && ./rebar compile"
end

task :develdeploy, :roles => :development do
  run "cd /home/dictionary_maker/dictionary_maker && /usr/bin/hg pull -u && ./rebar compile"
end

