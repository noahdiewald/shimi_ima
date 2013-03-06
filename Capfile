role :lingserver, "ling-staging-dictionary-maker"
role :production, "ling-production-dictionary-maker"

task :init, :roles => :lingserver do
  run "cd /home/dictionary_maker && /usr/bin/hg clone /hg/dictionary_maker && cd /home/dictionary_maker/dictionary_maker && ./rebar get-deps && ./rebar compile"
end

task :deploy, :roles => :lingserver do
  run "cd /home/dictionary_maker/dictionary_maker && 
       /usr/bin/hg pull && 
       /usr/bin/hg update staging && 
       rm ebin/* &&
       ./rebar get-deps &&
       ./rebar compile"
end

task :prodinit, :roles => :production do
  run "cd /home/dictionary_maker &&
      /usr/bin/hg clone ssh://repository.ling.wisc.edu//hg/dictionary_maker &&
      cd /home/dictionary_maker/dictionary_maker &&
      ./rebar get-deps &&
      ./rebar compile"
end

task :proddeploy, :roles => :production do
  run "cd /home/dictionary_maker/dictionary_maker &&
       /usr/bin/hg pull &&
       /usr/bin/hg update production &&
       rm ebin/* &&
       ./rebar get-deps &&
       ./rebar compile"
end

