role :lingserver, "ling-staging-dictionary-maker", "fcp-dictionary-maker"
role :production, "ling-production-dictionary-maker"

task :init, :roles => :lingserver do
  run "cd /home/dictionary_maker &&
      /usr/bin/git clone git@bitbucket.org:noahdiewald/shimi_ima.git &&
      cd /home/dictionary_maker/shimi_ima &&
      rebar get-deps &&
      rebar compile"
end

task :deploy, :roles => :lingserver do
  run "cd /home/dictionary_maker/shimi_ima && 
       /usr/bin/git pull && 
       /usr/bin/git checkout staging && 
       make distclean &&
       make build"
end

task :proddeploy, :roles => :production do
  run "cd /home/dictionary_maker/shimi_ima &&
       /usr/bin/git pull &&
       /usr/bin/git checkout production &&
       make distclean &&
       make build"
end
