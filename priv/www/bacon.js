  


<!DOCTYPE html>
<html>
  <head prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# githubog: http://ogp.me/ns/fb/githubog#">
    <meta charset='utf-8'>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <title>bacon.js/lib/Bacon.min.js at master · raimohanska/bacon.js</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub" />
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub" />
    <link rel="apple-touch-icon-precomposed" sizes="57x57" href="apple-touch-icon-114.png" />
    <link rel="apple-touch-icon-precomposed" sizes="114x114" href="apple-touch-icon-114.png" />
    <link rel="apple-touch-icon-precomposed" sizes="72x72" href="apple-touch-icon-144.png" />
    <link rel="apple-touch-icon-precomposed" sizes="144x144" href="apple-touch-icon-144.png" />
    <link rel="logo" type="image/svg" href="http://github-media-downloads.s3.amazonaws.com/github-logo.svg" />
    <meta name="msapplication-TileImage" content="/windows-tile.png">
    <meta name="msapplication-TileColor" content="#ffffff">

    
    
    <link rel="icon" type="image/x-icon" href="/favicon.ico" />

    <meta content="authenticity_token" name="csrf-param" />
<meta content="RJMLTldk3pJI0SNkdjxr672VBHI5+xL1q5r5Dq5om90=" name="csrf-token" />

    <link href="https://a248.e.akamai.net/assets.github.com/assets/github-76ac4efbc995c3037599255501cef3430cf58ba4.css" media="screen" rel="stylesheet" type="text/css" />
    <link href="https://a248.e.akamai.net/assets.github.com/assets/github2-6e435db7b63fa6c2a062d4026dc820e7b6254f34.css" media="screen" rel="stylesheet" type="text/css" />
    


        <script src="https://a248.e.akamai.net/assets.github.com/assets/frameworks-995182d02a0effa06cdc51e33fe6e729014a3c91.js" type="text/javascript"></script>
      <script src="https://a248.e.akamai.net/assets.github.com/assets/github-b7b1a5d7b2078ccc6c8ae00ad89094829357f88f.js" type="text/javascript"></script>
      

        <link rel='permalink' href='/raimohanska/bacon.js/blob/6f99a5df72011a0c3e9879fbe4a3bad7fe7cae68/lib/Bacon.min.js'>
    <meta property="og:title" content="bacon.js"/>
    <meta property="og:type" content="githubog:gitrepository"/>
    <meta property="og:url" content="https://github.com/raimohanska/bacon.js"/>
    <meta property="og:image" content="https://secure.gravatar.com/avatar/6aec4e7168bcc6ce45a24ab174e136fc?s=420&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png"/>
    <meta property="og:site_name" content="GitHub"/>
    <meta property="og:description" content="bacon.js - FRP (functional reactive programming) library for Javascript"/>
    <meta property="twitter:card" content="summary"/>
    <meta property="twitter:site" content="@GitHub">
    <meta property="twitter:title" content="raimohanska/bacon.js"/>

    <meta name="description" content="bacon.js - FRP (functional reactive programming) library for Javascript" />

  <link href="https://github.com/raimohanska/bacon.js/commits/master.atom" rel="alternate" title="Recent Commits to bacon.js:master" type="application/atom+xml" />

  </head>


  <body class="logged_in page-blob linux vis-public env-production ">
    <div id="wrapper">

      

      

      


        <div class="header header-logged-in true">
          <div class="container clearfix">

            <a class="header-logo-blacktocat" href="https://github.com/">
  <span class="mega-icon mega-icon-blacktocat"></span>
</a>

            <div class="divider-vertical"></div>

            
  <a href="/notifications" class="notification-indicator tooltipped downwards" title="You have unread notifications">
    <span class="mail-status unread"></span>
  </a>
  <div class="divider-vertical"></div>


              
  <div class="topsearch command-bar-activated">
    <form accept-charset="UTF-8" action="/search" class="command_bar_form" id="top_search_form" method="get">
  <a href="/search/advanced" class="advanced-search-icon tooltipped downwards command-bar-search" id="advanced_search" title="Advanced search"><span class="mini-icon mini-icon-advanced-search "></span></a>

  <input type="text" name="q" id="command-bar" placeholder="Search or type a command" tabindex="1" data-username="noahdiewald" autocapitalize="off">

  <span class="mini-icon help tooltipped downwards" title="Show command bar help">
    <span class="mini-icon mini-icon-help"></span>
  </span>

  <input type="hidden" name="ref" value="commandbar">

  <div class="divider-vertical"></div>
</form>

    <ul class="top-nav">
        <li class="explore"><a href="https://github.com/explore">Explore</a></li>
        <li><a href="https://gist.github.com">Gist</a></li>
        <li><a href="/blog">Blog</a></li>
      <li><a href="http://help.github.com">Help</a></li>
    </ul>
  </div>


            

  
    <ul id="user-links">
      <li>
        <a href="https://github.com/noahdiewald" class="name">
          <img height="20" src="https://secure.gravatar.com/avatar/9c6b70c2a1c96cd86f05e0058af916f9?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="20" /> noahdiewald
        </a>
      </li>
      <li>
        <a href="/new" id="new_repo" class="tooltipped downwards" title="Create a new repo">
          <span class="mini-icon mini-icon-create"></span>
        </a>
      </li>
      <li>
        <a href="/settings/profile" id="account_settings"
          class="tooltipped downwards"
          title="Account settings ">
          <span class="mini-icon mini-icon-account-settings"></span>
        </a>
      </li>
      <li>
          <a href="/logout" data-method="post" id="logout" class="tooltipped downwards" title="Sign out">
            <span class="mini-icon mini-icon-logout"></span>
          </a>
      </li>
    </ul>



            
          </div>
        </div>


      

      


            <div class="site hfeed" itemscope itemtype="http://schema.org/WebPage">
      <div class="hentry">
        
        <div class="pagehead repohead instapaper_ignore readability-menu">
          <div class="container">
            <div class="title-actions-bar">
              


                  <ul class="pagehead-actions">

          <li class="subscription">
              <form accept-charset="UTF-8" action="/notifications/subscribe" data-autosubmit="true" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="authenticity_token" type="hidden" value="RJMLTldk3pJI0SNkdjxr672VBHI5+xL1q5r5Dq5om90=" /></div>  <input id="repository_id" name="repository_id" type="hidden" value="3605299" />
  <div class="context-menu-container js-menu-container js-context-menu">
    <span class="minibutton switcher bigger js-menu-target">
      <span class="js-context-button">
          <span class="mini-icon mini-icon-watching"></span>Watch
      </span>
    </span>

    <div class="context-pane js-menu-content">
      <a href="#" class="close js-menu-close"><span class="mini-icon mini-icon-remove-close"></span></a>
      <div class="context-title">Notification status</div>

      <div class="context-body pane-selector">
        <ul class="js-navigation-container">
          <li class="selector-item js-navigation-item js-navigation-target selected">
            <span class="mini-icon mini-icon-confirm"></span>
            <label>
              <input checked="checked" id="do_included" name="do" type="radio" value="included" />
              <h4>Not watching</h4>
              <p>You will only receive notifications when you participate or are mentioned.</p>
            </label>
            <span class="context-button-text js-context-button-text">
              <span class="mini-icon mini-icon-watching"></span>
              Watch
            </span>
          </li>
          <li class="selector-item js-navigation-item js-navigation-target ">
            <span class="mini-icon mini-icon-confirm"></span>
            <label>
              <input id="do_subscribed" name="do" type="radio" value="subscribed" />
              <h4>Watching</h4>
              <p>You will receive all notifications for this repository.</p>
            </label>
            <span class="context-button-text js-context-button-text">
              <span class="mini-icon mini-icon-unwatch"></span>
              Unwatch
            </span>
          </li>
          <li class="selector-item js-navigation-item js-navigation-target ">
            <span class="mini-icon mini-icon-confirm"></span>
            <label>
              <input id="do_ignore" name="do" type="radio" value="ignore" />
              <h4>Ignored</h4>
              <p>You will not receive notifications for this repository.</p>
            </label>
            <span class="context-button-text js-context-button-text">
              <span class="mini-icon mini-icon-mute"></span>
              Stop ignoring
            </span>
          </li>
        </ul>
      </div>
    </div>
  </div>
</form>
          </li>

          <li class="js-toggler-container js-social-container starring-container on">
            <a href="/raimohanska/bacon.js/unstar" class="minibutton js-toggler-target starred" data-remote="true" data-method="post" rel="nofollow">
              <span class="mini-icon mini-icon-star"></span>Unstar
            </a><a href="/raimohanska/bacon.js/star" class="minibutton js-toggler-target unstarred" data-remote="true" data-method="post" rel="nofollow">
              <span class="mini-icon mini-icon-star"></span>Star
            </a><a class="social-count js-social-count" href="/raimohanska/bacon.js/stargazers">193</a>
          </li>

              <li><a href="/raimohanska/bacon.js/fork" class="minibutton js-toggler-target fork-button lighter" rel="nofollow" data-method="post"><span class="mini-icon mini-icon-fork"></span>Fork</a><a href="/raimohanska/bacon.js/network" class="social-count">23</a>
              </li>


    </ul>

              <h1 itemscope itemtype="http://data-vocabulary.org/Breadcrumb" class="entry-title public">
                <span class="repo-label"><span>public</span></span>
                <span class="mega-icon mega-icon-public-repo"></span>
                <span class="author vcard">
                  <a href="/raimohanska" class="url fn" itemprop="url" rel="author">
                  <span itemprop="title">raimohanska</span>
                  </a></span> /
                <strong><a href="/raimohanska/bacon.js" class="js-current-repository">bacon.js</a></strong>
              </h1>
            </div>

            

  <ul class="tabs">
    <li><a href="/raimohanska/bacon.js" class="selected" highlight="repo_sourcerepo_downloadsrepo_commitsrepo_tagsrepo_branches">Code</a></li>
    <li><a href="/raimohanska/bacon.js/network" highlight="repo_network">Network</a></li>
    <li><a href="/raimohanska/bacon.js/pulls" highlight="repo_pulls">Pull Requests <span class='counter'>2</span></a></li>

      <li><a href="/raimohanska/bacon.js/issues" highlight="repo_issues">Issues <span class='counter'>2</span></a></li>

      <li><a href="/raimohanska/bacon.js/wiki" highlight="repo_wiki">Wiki</a></li>


    <li><a href="/raimohanska/bacon.js/graphs" highlight="repo_graphsrepo_contributors">Graphs</a></li>


  </ul>
  
<div class="tabnav">

  <span class="tabnav-right">
    <ul class="tabnav-tabs">
          <li><a href="/raimohanska/bacon.js/tags" class="tabnav-tab" highlight="repo_tags">Tags <span class="counter ">6</span></a></li>
    </ul>
    
  </span>

  <div class="tabnav-widget scope">


    <div class="select-menu js-menu-container js-select-menu js-branch-menu">
      <a class="minibutton select-menu-button js-menu-target" data-hotkey="w" data-ref="master">
        <span class="mini-icon mini-icon-branch"></span>
        <i>branch:</i>
        <span class="js-select-button">master</span>
      </a>

      <div class="select-menu-modal-holder js-menu-content js-navigation-container js-select-menu-pane">

        <div class="select-menu-modal js-select-menu-pane">
          <div class="select-menu-header">
            <span class="select-menu-title">Switch branches/tags</span>
            <span class="mini-icon mini-icon-remove-close js-menu-close"></span>
          </div> <!-- /.select-menu-header -->

          <div class="select-menu-filters">
            <div class="select-menu-text-filter">
              <input type="text" id="commitish-filter-field" class="js-select-menu-text-filter js-filterable-field js-navigation-enable" placeholder="Find or create a branch…">
            </div> <!-- /.select-menu-text-filter -->
            <div class="select-menu-tabs">
              <ul>
                <li class="select-menu-tab">
                  <a href="#" data-filter="branches" class="js-select-menu-tab selected">Branches</a>
                </li>
                <li class="select-menu-tab">
                  <a href="#" data-filter="tags" class="js-select-menu-tab">Tags</a>
                </li>
              </ul>
            </div><!-- /.select-menu-tabs -->
          </div><!-- /.select-menu-filters -->

          <div class="select-menu-list js-filter-tab js-filter-branches" data-filterable-for="commitish-filter-field" data-filterable-type="substring">



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/exhausted-streams/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="exhausted-streams" rel="nofollow">exhausted-streams</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/jquery-eventtransformer/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="jquery-eventtransformer" rel="nofollow">jquery-eventtransformer</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/jquery-extra-params/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="jquery-extra-params" rel="nofollow">jquery-extra-params</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/map-filter-to-property/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="map-filter-to-property" rel="nofollow">map-filter-to-property</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/mapEnd/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="mapEnd" rel="nofollow">mapEnd</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target selected">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/master/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="master" rel="nofollow">master</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/property.scan/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="property.scan" rel="nofollow">property.scan</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/switchCase/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="switchCase" rel="nofollow">switchCase</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/without-dispatcher/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="without-dispatcher" rel="nofollow">without-dispatcher</a>

              </div> <!-- /.select-menu-item -->



              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>

                    <a href="/raimohanska/bacon.js/blob/worzone-extensions/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="worzone-extensions" rel="nofollow">worzone-extensions</a>

              </div> <!-- /.select-menu-item -->


              <div class="select-menu-no-results js-not-filterable">Nothing to show</div>
          </div> <!-- /.select-menu-list -->


          <div class="select-menu-list js-filter-tab js-filter-tags" data-filterable-for="commitish-filter-field" data-filterable-type="substring" style="display:none;">

              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>
                    <a href="/raimohanska/bacon.js/blob/0.0.9/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="0.0.9" rel="nofollow">0.0.9</a>

              </div> <!-- /.select-menu-item -->
              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>
                    <a href="/raimohanska/bacon.js/blob/0.0.8/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="0.0.8" rel="nofollow">0.0.8</a>

              </div> <!-- /.select-menu-item -->
              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>
                    <a href="/raimohanska/bacon.js/blob/0.0.7/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="0.0.7" rel="nofollow">0.0.7</a>

              </div> <!-- /.select-menu-item -->
              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>
                    <a href="/raimohanska/bacon.js/blob/0.0.6/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="0.0.6" rel="nofollow">0.0.6</a>

              </div> <!-- /.select-menu-item -->
              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>
                    <a href="/raimohanska/bacon.js/blob/0.0.5/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="0.0.5" rel="nofollow">0.0.5</a>

              </div> <!-- /.select-menu-item -->
              <div class="select-menu-item js-navigation-item js-navigation-target ">
                <span class="select-menu-checkmark mini-icon mini-icon-confirm"></span>
                    <a href="/raimohanska/bacon.js/blob/0.0.4/lib/Bacon.min.js" class="js-navigation-open select-menu-item-text js-select-button-text" data-name="0.0.4" rel="nofollow">0.0.4</a>

              </div> <!-- /.select-menu-item -->

            <div class="select-menu-no-results js-not-filterable">Nothing to show</div>

          </div> <!-- /.select-menu-list -->

        </div> <!-- /.select-menu-modal -->
      </div> <!-- /.select-menu-modal-holder -->
    </div> <!-- /.select-menu -->

  </div> <!-- /.scope -->

  <ul class="tabnav-tabs">
    <li><a href="/raimohanska/bacon.js" class="selected tabnav-tab" highlight="repo_source">Files</a></li>
    <li><a href="/raimohanska/bacon.js/commits/master" class="tabnav-tab" highlight="repo_commits">Commits</a></li>
    <li><a href="/raimohanska/bacon.js/branches" class="tabnav-tab" highlight="repo_branches" rel="nofollow">Branches <span class="counter ">10</span></a></li>
  </ul>

</div>

  
  
  


            
          </div>
        </div><!-- /.repohead -->

        <div id="js-repo-pjax-container" class="container context-loader-container" data-pjax-container>
          


<!-- blob contrib key: blob_contributors:v21:125c6349872652dce8f55bc54214b101 -->
<!-- blob contrib frag key: views10/v8/blob_contributors:v21:125c6349872652dce8f55bc54214b101 -->

<div id="slider">
    <div class="frame-meta">

      <p title="This is a placeholder element" class="js-history-link-replace hidden"></p>
      <div class="breadcrumb">
        <span class='bold'><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/raimohanska/bacon.js" class="js-slide-to" data-direction="back" itemscope="url"><span itemprop="title">bacon.js</span></a></span></span> / <span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/raimohanska/bacon.js/tree/master/lib" class="js-slide-to" data-direction="back" itemscope="url"><span itemprop="title">lib</span></a></span> / <strong class="final-path">Bacon.min.js</strong> <span class="js-zeroclipboard zeroclipboard-button" data-clipboard-text="lib/Bacon.min.js" data-copied-hint="copied!" title="copy to clipboard"><span class="mini-icon mini-icon-clipboard"></span></span>
      </div>

      <a href="/raimohanska/bacon.js/find/master" class="js-slide-to" data-hotkey="t" style="display:none">Show File Finder</a>

        
  <div class="commit file-history-tease">
    <img class="main-avatar" height="24" src="https://secure.gravatar.com/avatar/2a2926c30016ded311d978f1ec504110?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
    <span class="author"><span rel="author">Juha Paananen</span></span>
    <time class="js-relative-date" datetime="2013-01-17T23:32:18-08:00" title="2013-01-17 23:32:18">January 17, 2013</time>
    <div class="commit-title">
        <a href="/raimohanska/bacon.js/commit/da1f3964e1181d81d85160f8f84ea51d63d955fb" class="message">build js</a>
    </div>

    <div class="participation">
      <p class="quickstat"><a href="#blob_contributors_box" rel="facebox"><strong>2</strong> contributors</a></p>
          <a class="avatar tooltipped downwards" title="raimohanska" href="/raimohanska/bacon.js/commits/master/lib/Bacon.min.js?author=raimohanska"><img height="20" src="https://secure.gravatar.com/avatar/6aec4e7168bcc6ce45a24ab174e136fc?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="20" /></a>
    <a class="avatar tooltipped downwards" title="shangaslammi" href="/raimohanska/bacon.js/commits/master/lib/Bacon.min.js?author=shangaslammi"><img height="20" src="https://secure.gravatar.com/avatar/7b716e0eaef76c721a65ff76a3a4e596?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="20" /></a>


    </div>
    <div id="blob_contributors_box" style="display:none">
      <h2>Users on GitHub who have contributed to this file</h2>
      <ul class="facebox-user-list">
        <li>
          <img height="24" src="https://secure.gravatar.com/avatar/6aec4e7168bcc6ce45a24ab174e136fc?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
          <a href="/raimohanska">raimohanska</a>
        </li>
        <li>
          <img height="24" src="https://secure.gravatar.com/avatar/7b716e0eaef76c721a65ff76a3a4e596?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
          <a href="/shangaslammi">shangaslammi</a>
        </li>
      </ul>
    </div>
  </div>


    </div><!-- ./.frame-meta -->

    <div class="frames">
      <div class="frame" data-permalink-url="/raimohanska/bacon.js/blob/6f99a5df72011a0c3e9879fbe4a3bad7fe7cae68/lib/Bacon.min.js" data-title="bacon.js/lib/Bacon.min.js at master · raimohanska/bacon.js · GitHub" data-type="blob">

        <div id="files" class="bubble">
          <div class="file">
            <div class="meta">
              <div class="info">
                <span class="icon"><b class="mini-icon mini-icon-text-file"></b></span>
                <span class="mode" title="File Mode">file</span>
                  <span>2 lines (2 sloc)</span>
                <span>19.467 kb</span>
              </div>
              <ul class="button-group actions">
                  <li>
                        <a class="grouped-button minibutton bigger lighter tooltipped leftwards"
                           title="Clicking this button will automatically fork this project so you can edit the file"
                           href="/raimohanska/bacon.js/edit/master/lib/Bacon.min.js"
                           data-method="post" rel="nofollow">Edit</a>
                  </li>
                <li><a href="/raimohanska/bacon.js/raw/master/lib/Bacon.min.js" class="button minibutton grouped-button bigger lighter" id="raw-url">Raw</a></li>
                  <li><a href="/raimohanska/bacon.js/blame/master/lib/Bacon.min.js" class="button minibutton grouped-button bigger lighter">Blame</a></li>
                <li><a href="/raimohanska/bacon.js/commits/master/lib/Bacon.min.js" class="button minibutton grouped-button bigger lighter" rel="nofollow">History</a></li>
              </ul>

            </div>
                <div class="data type-javascript js-blob-data">
      <table cellpadding="0" cellspacing="0" class="lines">
        <tr>
          <td>
            <pre class="line_numbers"><span id="L1" rel="#L1">1</span>
<span id="L2" rel="#L2">2</span>
</pre>
          </td>
          <td width="100%">
                <div class="highlight"><pre><div class='line' id='LC1'>// Generated by CoffeeScript 1.3.3</div><div class='line' id='LC2'>((function(){var a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,N=[].slice,O={}.hasOwnProperty,P=function(a,b){function d(){this.constructor=a}for(var c in b)O.call(b,c)&amp;&amp;(a[c]=b[c]);return d.prototype=b.prototype,a.prototype=new d,a.__super__=b.prototype,a},Q=function(a,b){return function(){return a.apply(b,arguments)}};(M=this.jQuery||this.Zepto)!=null&amp;&amp;(M.fn.asEventStream=function(b,c,d){var e;return d==null&amp;&amp;(d=L.id),w(c)&amp;&amp;(d=c,c=null),e=this,new g(function(f){var g,h;return g=function(){var b,c;b=1&lt;=arguments.length?N.call(arguments,0):[],c=f(A(d.apply(null,b)));if(c===a.noMore)return h()},h=function(){return e.off(b,c,g)},e.on(b,c,g),h})}),a=this.Bacon={},a.fromPromise=function(b){return new a.EventStream(function(a){var c,f;return f=function(b){return a(new i(b)),a(new d)},c=function(b){return a(new e(b)),a(new d)},b.then(f,c),B})},a.noMore=&quot;veggies&quot;,a.more=&quot;moar bacon!&quot;,a.later=function(b,c){return a.sequentially(b,[c])},a.sequentially=function(b,c){var d,e;return d=-1,e=function(){return d++,d&lt;c.length?G(c[d]):r()},a.fromPoll(b,e)},a.repeatedly=function(b,c){var d,e;return d=-1,e=function(){return d++,G(c[d%c.length])},a.fromPoll(b,e)},a.fromCallback=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b=y(b,a),new g(function(a){var c;return c=function(b){return a(A(b)),a(r())},b(c),B})},a.fromPoll=function(b,c){return new g(function(d){var e,f,g;return f=void 0,e=function(){var b,e;e=c(),b=d(e);if(b===a.noMore||e.isEnd())return g()},g=function(){return clearInterval(f)},f=setInterval(e,b),g})},a.fromEventTarget=function(b,c){return new g(function(d){var e,f;return e=function(b){var c;c=d(A(b));if(c===a.noMore)return f()},b.addEventListener?(f=function(){return b.removeEventListener(c,e,!1)},b.addEventListener(c,e,!1)):(f=function(){return b.removeListener(c,e)},b.addListener(c,e)),f})},a.interval=function(b,c){var d;return c==null&amp;&amp;(c={}),d=function(){return A(c)},a.fromPoll(b,d)},a.constant=function(a){return new l(E([a],t))},a.never=function(){return a.fromArray([])},a.once=function(b){return a.fromArray([b])},a.fromArray=function(a){return new g(E(a,A))},E=function(a,b){return function(c){var d,e,f;for(e=0,f=a.length;e&lt;f;e++)d=a[e],c(b(d));return c(r()),B}},a.combineAll=function(a,b){var c,d,e,f,g;d=L.head(a),g=L.tail(a);for(e=0,f=g.length;e&lt;f;e++)c=g[e],d=b(d,c);return d},a.mergeAll=function(b){return a.combineAll(b,function(a,b){return a.merge(b)})},a.combineAsArray=function(){var b,c,d,e,f,g,h;e=arguments[0],b=2&lt;=arguments.length?N.call(arguments,1):[],e instanceof Array||(e=[e].concat(b));if(e.length){d=L.head(e).toProperty().map(function(a){return[a]}),h=L.tail(e);for(f=0,g=h.length;f&lt;g;f++)c=h[f],d=d.combine(c,function(a,b){return a.concat([b])});return d}return a.constant([])},a.combineWith=function(b,c){return a.combineAll(b,function(a,b){return a.toProperty().combine(b,c)})},a.combineTemplate=function(b){var c,d,e,f,g,h,i,j,l,m;return i=[],m=[],h=function(a){return a[a.length-1]},l=function(a,b,c){return h(a)[b]=c},c=function(a,b){return function(c,d){return l(c,a,d[b])}},g=function(a,b){return function(c,d){return l(c,a,b)}},j=function(a){return a instanceof Array?[]:{}},e=function(a,b){var d,e;return b instanceof k?(m.push(b),i.push(c(a,m.length-1))):typeof b==&quot;object&quot;?(e=function(a){return function(c,d){var e;return e=j(b),l(c,a,e),c.push(e)}},d=function(a,b){return a.pop()},i.push(e(a)),f(b),i.push(d)):i.push(g(a,b))},f=function(a){return L.each(a,e)},f(b),d=function(a){var c,d,e,f,g;e=j(b),c=[e];for(f=0,g=i.length;f&lt;g;f++)d=i[f],d(c,a);return e},a.combineAsArray(m).map(d)},a.latestValue=function(a){var b,c=this;return b=void 0,a.subscribe(function(a){if(a.hasValue())return b=a.value}),function(){return b}},f=function(){function a(){}return a.prototype.isEvent=function(){return!0},a.prototype.isEnd=function(){return!1},a.prototype.isInitial=function(){return!1},a.prototype.isNext=function(){return!1},a.prototype.isError=function(){return!1},a.prototype.hasValue=function(){return!1},a.prototype.filter=function(a){return!0},a.prototype.getOriginalEvent=function(){return this.sourceEvent!=null?this.sourceEvent.getOriginalEvent():this},a.prototype.onDone=function(a){return a()},a}(),i=function(a){function b(a,b){this.value=a}return P(b,a),b.prototype.isNext=function(){return!0},b.prototype.hasValue=function(){return!0},b.prototype.fmap=function(a){return this.apply(a(this.value))},b.prototype.apply=function(a){return A(a,this.getOriginalEvent())},b.prototype.filter=function(a){return a(this.value)},b.prototype.describe=function(){return this.value},b}(f),h=function(a){function b(){return b.__super__.constructor.apply(this,arguments)}return P(b,a),b.prototype.isInitial=function(){return!0},b.prototype.isNext=function(){return!1},b.prototype.apply=function(a){return t(a,this.getOriginalEvent())},b}(i),d=function(a){function b(){return b.__super__.constructor.apply(this,arguments)}return P(b,a),b.prototype.isEnd=function(){return!0},b.prototype.fmap=function(){return this},b.prototype.apply=function(){return this},b.prototype.describe=function(){return&quot;&lt;end&gt;&quot;},b}(f),e=function(a){function b(a){this.error=a}return P(b,a),b.prototype.isError=function(){return!0},b.prototype.fmap=function(){return this},b.prototype.apply=function(){return this},b.prototype.describe=function(){return&quot;&lt;error&gt; &quot;+this.error},b}(f),k=function(){function b(){this.flatMapLatest=Q(this.flatMapLatest,this),this[&quot;switch&quot;]=Q(this[&quot;switch&quot;],this),this.scan=Q(this.scan,this),this.takeUntil=Q(this.takeUntil,this),this.assign=this.onValue}return b.prototype.onValue=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b=y(b,a),this.subscribe(function(a){if(a.hasValue())return b(a.value)})},b.prototype.onValues=function(a){return this.onValue(function(b){return a.apply(null,b)})},b.prototype.onError=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b=y(b,a),this.subscribe(function(a){if(a.isError())return b(a.error)})},b.prototype.onEnd=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b=y(b,a),this.subscribe(function(a){if(a.isEnd())return b()})},b.prototype.errors=function(){return this.filter(function(){return!1})},b.prototype.filter=function(){var b,c;return c=arguments[0],b=2&lt;=arguments.length?N.call(arguments,1):[],c=y(c,b),this.withHandler(function(b){return b.filter(c)?this.push(b):a.more})},b.prototype.takeWhile=function(){var b,c;return c=arguments[0],b=2&lt;=arguments.length?N.call(arguments,1):[],c=y(c,b),this.withHandler(function(b){return b.filter(c)?this.push(b):(this.push(r()),a.noMore)})},b.prototype.endOnError=function(){return this.withHandler(function(a){return a.isError()?(this.push(a),this.push(r())):this.push(a)})},b.prototype.take=function(b){return this.withHandler(function(c){return c.hasValue()?b===1?(this.push(c),this.push(r()),a.noMore):(b--,this.push(c)):this.push(c)})},b.prototype.map=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b=y(b,a),this.withHandler(function(a){return this.push(a.fmap(b))})},b.prototype.mapError=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b=y(b,a),this.withHandler(function(a){return a.isError()?this.push(A(b(a.error))):this.push(a)})},b.prototype[&quot;do&quot;]=function(){var a;return a=1&lt;=arguments.length?N.call(arguments,0):[],this.doAction.apply(this,a)},b.prototype.doAction=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b=y(b,a),this.withHandler(function(a){return a.hasValue()&amp;&amp;b(a.value),this.push(a)})},b.prototype.takeUntil=function(b){var c;return c=this,this.withSubscribe(function(d){var e,f,g,h,i,j;return j=!1,h=B,i=B,g=function(){return h(),i(),j=!0},e=function(b){return b.isEnd()?(i(),d(b),a.noMore):(b.getOriginalEvent().onDone(function(){var c;if(!j){c=d(b);if(c===a.noMore)return g()}}),a.more)},f=function(b){return b.isError()?a.more:b.isEnd()?a.noMore:(h(),d(r()),a.noMore)},h=c.subscribe(e),j||(i=b.subscribe(f)),g})},b.prototype.skip=function(b){return this.withHandler(function(c){return c.hasValue()?b&gt;0?(b--,a.more):this.push(c):this.push(c)})},b.prototype.distinctUntilChanged=function(){return this.skipDuplicates()},b.prototype.skipDuplicates=function(a){return a==null&amp;&amp;(a=function(a,b){return a===b}),this.withStateMachine(void 0,function(b,c){return c.hasValue()?a(b,c.value)?[b,[]]:[c.value,[c]]:[b,[c]]})},b.prototype.withStateMachine=function(b,c){var d;return d=b,this.withHandler(function(b){var e,f,g,h,i,j,k;e=c(d,b),f=e[0],h=e[1],d=f,i=a.more;for(j=0,k=h.length;j&lt;k;j++){g=h[j],i=this.push(g);if(i===a.noMore)return i}return i})},b.prototype.scan=function(b,c){var d,e,f=this;return c=F(c),d=J(b),e=function(b){var e,g;return e=!1,g=f.subscribe(function(f){return f.hasValue()?e&amp;&amp;f.isInitial()?a.more:(e=!0,d=new n(c(d.getOrElse(void 0),f.value)),b(f.apply(d.get()))):(f.isEnd()&amp;&amp;(e=!0),b(f))}),e||d.forEach(function(c){var d;d=b(t(c));if(d===a.noMore)return g(),g=B}),g},new l((new m(e)).subscribe)},b.prototype.flatMap=function(b){var c;return c=this,new g(function(d){var e,f,g,h,i,j;return f=[],g=!1,j=function(){},i=function(){var a,b,c;j();for(b=0,c=f.length;b&lt;c;b++)a=f[b],a();return f=[]},e=function(){if(g&amp;&amp;f.length===0)return d(r())},h=function(c){var h,j,k,l,m;if(c.isEnd())return g=!0,e();if(c.isError())return d(c);h=b(c.value),m=void 0,j=!1,l=function(){return m!=null&amp;&amp;D(m,f),e()},k=function(b){var c;return b.isEnd()?(l(),j=!0,a.noMore):(c=d(b),c===a.noMore&amp;&amp;i(),c)},m=h.subscribe(k);if(!j)return f.push(m)},j=c.subscribe(h),i})},b.prototype[&quot;switch&quot;]=function(){var a;return a=1&lt;=arguments.length?N.call(arguments,0):[],this.flatMapLatest.apply(this,a)},b.prototype.flatMapLatest=function(a){var b,c=this;return b=this.toEventStream(),b.flatMap(function(c){return a(c).takeUntil(b)})},b.prototype.not=function(){return this.map(function(a){return!a})},b.prototype.log=function(){return this.subscribe(function(a){return console.log(a.describe())}),this},b.prototype.slidingWindow=function(a){return this.scan([],function(b,c){return b.concat([c]).slice(-a)})},b}(),g=function(b){function d(a){var b;d.__super__.constructor.call(this),b=new c(a),this.subscribe=b.subscribe,this.hasSubscribers=b.hasSubscribers}return P(d,b),d.prototype.map=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b instanceof l?b.sampledBy(this,s):d.__super__.map.apply(this,[b].concat(N.call(a)))},d.prototype.filter=function(){var a,b;return b=arguments[0],a=2&lt;=arguments.length?N.call(arguments,1):[],b instanceof l?b.sampledBy(this,function(a,b){return[a,b]}).filter(function(a){var b,c;return b=a[0],c=a[1],b}).map(function(a){var b,c;return b=a[0],c=a[1],c}):d.__super__.filter.apply(this,[b].concat(N.call(a)))},d.prototype.delay=function(b){return this.flatMap(function(c){return a.later(b,c)})},d.prototype.throttle=function(b){return this.flatMapLatest(function(c){return a.later(b,c)})},d.prototype.bufferWithTime=function(b){var c,d,e,f;return f=[],e=function(a){return f.push(a),f.length===1},d=function(){var a;return a=f,f=[],a},c=function(){return a.later(b).map(d)},this.filter(e).flatMap(c)},d.prototype.bufferWithCount=function(a){var b;return b=[],this.withHandler(function(c){var d,e=this;d=function(){return e.push(A(b,c)),b=[]};if(c.isError())return this.push(c);if(c.isEnd())return d(),this.push(c);b.push(c.value);if(b.length===a)return d()})},d.prototype.merge=function(b){var c;return c=this,new d(function(d){var e,f,g,h,i,j;return h=B,i=B,j=!1,g=function(){return h(),i(),j=!0},e=0,f=function(b){var c;return b.isEnd()?(e++,e===2?d(r()):a.more):(c=d(b),c===a.noMore&amp;&amp;g(),c)},h=c.subscribe(f),j||(i=b.subscribe(f)),g})},d.prototype.toProperty=function(a){return arguments.length===0&amp;&amp;(a=j),this.scan(a,x)},d.prototype.toEventStream=function(){return this},d.prototype.concat=function(a){var b;return b=this,new d(function(c){var d;return d=b.subscribe(function(b){return b.isEnd()?d=a.subscribe(c):c(b)}),function(){return d()}})},d.prototype.startWith=function(b){return a.once(b).concat(this)},d.prototype.decorateWith=function(a,b){return b.sampledBy(this,function(b,c){var d;return d=q(c),d[a]=b,d})},d.prototype.mapEnd=function(){var b,c;return c=arguments[0],b=2&lt;=arguments.length?N.call(arguments,1):[],c=y(c,b),this.withHandler(function(b){return b.isEnd()?(this.push(A(c(b))),this.push(r()),a.noMore):this.push(b)})},d.prototype.withHandler=function(a){var b;return b=new c(this.subscribe,a),new d(b.subscribe)},d.prototype.withSubscribe=function(a){return new d(a)},d}(k),l=function(b){function c(b){var d,e=this;this.subscribe=b,this.toEventStream=Q(this.toEventStream,this),this.toProperty=Q(this.toProperty,this),this.changes=Q(this.changes,this),this.sample=Q(this.sample,this),c.__super__.constructor.call(this),d=function(b,d,f){var g,h;return g=j,h=j,new c(function(c){var i,j,k,l,m,o,p,q,s,t,u;return u=!1,s=B,t=B,q=function(){return s(),t(),u=!0},l=!1,o=!1,i=function(){var b;if(l&amp;&amp;o)return b=c(r()),b===a.noMore&amp;&amp;q(),b},k=!1,j=function(b,d,e){return function(f){var j;return f.isEnd()?(b(),i(),a.noMore):f.isError()?(j=c(f),j===a.noMore&amp;&amp;q(),j):(d(new n(f.value)),g.isDefined&amp;&amp;h.isDefined?k&amp;&amp;f.isInitial()?a.more:(k=!0,j=e(c,f,g.value,h.value),j===a.noMore&amp;&amp;q(),j):a.more)}},m=j(function(){return l=!0},function(a){return g=a},d),p=j(function(){return o=!0},function(a){return h=a},f),s=e.subscribe(m),u||(t=b.subscribe(p)),q})},this.combine=function(a,b){var c,e;return c=F(b),e=function(a,b,d,e){return a(b.apply(c(d,e)))},d(a,e,e)},this.sampledBy=function(a,b){var c;return b==null&amp;&amp;(b=s),b=F(b),c=function(a,c,d,e){return a(c.apply(b(d,e)))},d(a,B,c).changes().takeUntil(a.filter(!1).mapEnd())}}return P(c,b),c.prototype.sample=function(b){return this.sampledBy(a.interval(b,{}))},c.prototype.changes=function(){var a=this;return new g(function(b){return a.subscribe(function(a){if(!a.isInitial())return b(a)})})},c.prototype.withHandler=function(a){return new c((new m(this.subscribe,a)).subscribe)},c.prototype.withSubscribe=function(a){return new c((new m(a)).subscribe)},c.prototype.toProperty=function(){return this},c.prototype.toEventStream=function(){var a=this;return new g(function(b){return a.subscribe(function(a){return a.isInitial()&amp;&amp;(a=A(a.value)),b(a)})})},c.prototype.and=function(a){return this.combine(a,function(a,b){return a&amp;&amp;b})},c.prototype.or=function(a){return this.combine(a,function(a,b){return a||b})},c.prototype.decode=function(b){return this.combine(a.combineTemplate(b),function(a,b){return b[a]})},c.prototype.delay=function(a){return o(this,this.changes().delay(a))},c.prototype.throttle=function(a){return o(this,this.changes().throttle(a))},c}(k),o=function(b,c){var d;return d=function(b){var c;return c=j,b.subscribe(function(b){return b.isInitial()&amp;&amp;(c=new n(b.value)),a.noMore}),c},c.toProperty(d(b))},c=function(){function b(b,c){var d,e,g,h,i=this;b==null&amp;&amp;(b=function(){return B}),g=[],d=!1,this.hasSubscribers=function(){return g.length&gt;0},h=B,e=function(a){return D(a,g)},this.push=function(b){var c,d,h,j,k,l,m;j=void 0,c=function(){var a,c,d,e;if(j!=null){c=j,j=void 0;for(d=0,e=c.length;d&lt;e;d++)a=c[d],a()}return b.onDone=f.prototype.onDone},b.onDone=function(a){return j!=null&amp;&amp;!L.contains(j,a)?j.push(a):j=[a]},m=p(g);for(k=0,l=m.length;k&lt;l;k++)h=m[k],d=h(b),(d===a.noMore||b.isEnd())&amp;&amp;e(h);return c(),i.hasSubscribers()?a.more:a.noMore},c==null&amp;&amp;(c=function(a){return this.push(a)}),this.handleEvent=function(a){return a.isEnd()&amp;&amp;(d=!0),c.apply(i,[a])},this.subscribe=function(a){return d?(a(r()),B):(g.push(a),g.length===1&amp;&amp;(h=b(i.handleEvent)),function(){e(a);if(!i.hasSubscribers())return h()})}}return b}(),m=function(b){function c(b,d){var e,f,g,h=this;c.__super__.constructor.call(this,b,d),e=j,g=this.push,b=this.subscribe,f=!1,this.push=function(a){return a.isEnd()&amp;&amp;(f=!0),a.hasValue()&amp;&amp;(e=new n(a.value)),g.apply(h,[a])},this.subscribe=function(c){var d,g,i;return d=!1,i=function(){return h.hasSubscribers()||f},g=e.filter(i).map(function(a){return c(t(a))}),g.getOrElse(a.more)===a.noMore?B:f?(c(r()),B):b.apply(h,[c])}}return P(c,b),c}(c),b=function(b){function d(){var b,f,g,h,i,j,k,l,m,n=this;i=void 0,m=[],h=[],f=!1,g=function(b){return function(c){return c.isEnd()?(D(b,h),a.noMore):i(c)}},l=function(){var a,b,c;for(b=0,c=m.length;b&lt;c;b++)a=m[b],a();return m=[]},j=function(a){var b,c,d,e;i=a,m=[],e=p(h);for(c=0,d=e.length;c&lt;d;c++)b=e[c],m.push(b.subscribe(g(b)));return l},b=new c(j),k=function(a){return b.subscribe(a)},d.__super__.constructor.call(this,k),this.plug=function(a){if(f)return;h.push(a);if(i!=null)return m.push(a.subscribe(g(a)))},this.push=function(a){if(i!=null)return i(A(a))},this.error=function(a){if(i!=null)return i(new e(a))},this.end=function(){f=!0,l();if(i!=null)return i(r())}}return P(d,b),d}(g),n=function(){function a(a){this.value=a}return a.prototype.getOrElse=function(){return this.value},a.prototype.get=function(){return this.value},a.prototype.filter=function(b){return b(this.value)?new a(this.value):j},a.prototype.map=function(b){return new a(b(this.value))},a.prototype.forEach=function(a){return a(this.value)},a.prototype.isDefined=!0,a.prototype.toArray=function(){return[this.value]},a}(),j={getOrElse:function(a){return a},filter:function(){return j},map:function(){return j},forEach:function(){},isDefined:!1,toArray:function(){return[]}},a.EventStream=g,a.Property=l,a.Observable=k,a.Bus=b,a.Initial=h,a.Next=i,a.End=d,a.Error=e,B=function(){},x=function(a,b){return b},s=function(a,b){return a},t=function(a){return new h(a)},A=function(a){return new i(a)},r=function(){return new d},u=function(a){return a!=null&amp;&amp;a.isEvent!=null&amp;&amp;a.isEvent()},G=function(a){return u(a)?a:A(a)},p=function(a){return a.slice(0)},q=function(a){var b,c,d;b={};for(c in a)d=a[c],b[c]=d;return b},D=function(a,b){var c;c=b.indexOf(a);if(c&gt;=0)return b.splice(c,1)},w=function(a){return typeof a==&quot;function&quot;},z=function(a,b,c){return c===void 0&amp;&amp;(c=[]),function(d){return a[b].apply(a,c.concat([d]))}},C=function(a,b){return function(c){return a.apply(null,b.concat([c]))}},y=function(a,b){return w(a)?b.length?C(a,b):a:v(a)?H(a,b):typeof a==&quot;object&quot;&amp;&amp;b.length?z(a,L.head(b),L.tail(b)):L.always(a)},v=function(a){return typeof a==&quot;string&quot;&amp;&amp;a.length&gt;1&amp;&amp;a[0]===&quot;.&quot;},H=function(a,b){var c,d;return d=a.slice(1).split(&quot;.&quot;),c=L.map(K(b),d),function(b){var d,e;for(d=0,e=c.length;d&lt;e;d++)a=c[d],b=a(b);return b}},K=function(a){return function(b){return function(c){var d;return d=c[b],w(d)?d.apply(c,a):d}}},I=function(a){return a.slice(1)},F=function(a){var b;if(w(a))return a;if(v(a))return b=I(a),function(a,c){return a[b](c)}},J=function(a){return a instanceof n||a===j?a:new n(a)},typeof define!=&quot;undefined&quot;&amp;&amp;define!==null&amp;&amp;define.amd!=null&amp;&amp;typeof define==&quot;function&quot;&amp;&amp;define(function(){return a}),L={head:function(a){return a[0]},always:function(a){return function(){return a}},empty:function(a){return a.length===0},tail:function(a){return a.slice(1,a.length)},filter:function(a,b){var c,d,e,f;c=[];for(e=0,f=b.length;e&lt;f;e++)d=b[e],a(d)&amp;&amp;c.push(d);return c},map:function(a,b){var c,d,e,f;f=[];for(d=0,e=b.length;d&lt;e;d++)c=b[d],f.push(a(c));return f},each:function(a,b){var c,d,e;e=[];for(c in a)d=a[c],e.push(b(c,d));return e},contains:function(a,b){return a.indexOf(b)&gt;=0},id:function(a){return a},last:function(a){return a[a.length-1]}},a._=L})).call(this);</div></pre></div>
          </td>
        </tr>
      </table>
  </div>

          </div>
        </div>

        <a href="#jump-to-line" rel="facebox" data-hotkey="l" class="js-jump-to-line" style="display:none">Jump to Line</a>
        <div id="jump-to-line" style="display:none">
          <h2>Jump to Line</h2>
          <form accept-charset="UTF-8" class="js-jump-to-line-form">
            <input class="textfield js-jump-to-line-field" type="text">
            <div class="full-button">
              <button type="submit" class="button">Go</button>
            </div>
          </form>
        </div>

      </div>
    </div>
</div>

<div id="js-frame-loading-template" class="frame frame-loading large-loading-area" style="display:none;">
  <img class="js-frame-loading-spinner" src="https://a248.e.akamai.net/assets.github.com/images/spinners/octocat-spinner-128.gif?1347543527" height="64" width="64">
</div>


        </div>
      </div>
      <div class="context-overlay"></div>
    </div>

      <div id="footer-push"></div><!-- hack for sticky footer -->
    </div><!-- end of wrapper - hack for sticky footer -->

      <!-- footer -->
      <div id="footer">
  <div class="container clearfix">

      <dl class="footer_nav">
        <dt>GitHub</dt>
        <dd><a href="https://github.com/about">About us</a></dd>
        <dd><a href="https://github.com/blog">Blog</a></dd>
        <dd><a href="https://github.com/contact">Contact &amp; support</a></dd>
        <dd><a href="http://enterprise.github.com/">GitHub Enterprise</a></dd>
        <dd><a href="http://status.github.com/">Site status</a></dd>
      </dl>

      <dl class="footer_nav">
        <dt>Applications</dt>
        <dd><a href="http://mac.github.com/">GitHub for Mac</a></dd>
        <dd><a href="http://windows.github.com/">GitHub for Windows</a></dd>
        <dd><a href="http://eclipse.github.com/">GitHub for Eclipse</a></dd>
        <dd><a href="http://mobile.github.com/">GitHub mobile apps</a></dd>
      </dl>

      <dl class="footer_nav">
        <dt>Services</dt>
        <dd><a href="http://get.gaug.es/">Gauges: Web analytics</a></dd>
        <dd><a href="http://speakerdeck.com">Speaker Deck: Presentations</a></dd>
        <dd><a href="https://gist.github.com">Gist: Code snippets</a></dd>
        <dd><a href="http://jobs.github.com/">Job board</a></dd>
      </dl>

      <dl class="footer_nav">
        <dt>Documentation</dt>
        <dd><a href="http://help.github.com/">GitHub Help</a></dd>
        <dd><a href="http://developer.github.com/">Developer API</a></dd>
        <dd><a href="http://github.github.com/github-flavored-markdown/">GitHub Flavored Markdown</a></dd>
        <dd><a href="http://pages.github.com/">GitHub Pages</a></dd>
      </dl>

      <dl class="footer_nav">
        <dt>More</dt>
        <dd><a href="http://training.github.com/">Training</a></dd>
        <dd><a href="https://github.com/edu">Students &amp; teachers</a></dd>
        <dd><a href="http://shop.github.com">The Shop</a></dd>
        <dd><a href="/plans">Plans &amp; pricing</a></dd>
        <dd><a href="http://octodex.github.com/">The Octodex</a></dd>
      </dl>

      <hr class="footer-divider">


    <p class="right">&copy; 2013 <span title="0.07717s from fe16.rs.github.com">GitHub</span> Inc. All rights reserved.</p>
    <a class="left" href="https://github.com/">
      <span class="mega-icon mega-icon-invertocat"></span>
    </a>
    <ul id="legal">
        <li><a href="https://github.com/site/terms">Terms of Service</a></li>
        <li><a href="https://github.com/site/privacy">Privacy</a></li>
        <li><a href="https://github.com/security">Security</a></li>
    </ul>

  </div><!-- /.container -->

</div><!-- /.#footer -->


    

    

<div id="keyboard_shortcuts_pane" class="instapaper_ignore readability-extra" style="display:none">
  <h2>Keyboard Shortcuts <small><a href="#" class="js-see-all-keyboard-shortcuts">(see all)</a></small></h2>

  <div class="columns threecols">
    <div class="column first">
      <h3>Site wide shortcuts</h3>
      <dl class="keyboard-mappings">
        <dt>s</dt>
        <dd>Focus command bar</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>?</dt>
        <dd>Bring up this help dialog</dd>
      </dl>
    </div><!-- /.column.first -->

    <div class="column middle" style='display:none'>
      <h3>Commit list</h3>
      <dl class="keyboard-mappings">
        <dt>j</dt>
        <dd>Move selection down</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>k</dt>
        <dd>Move selection up</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>c <em>or</em> o <em>or</em> enter</dt>
        <dd>Open commit</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>y</dt>
        <dd>Expand URL to its canonical form</dd>
      </dl>
    </div><!-- /.column.first -->

    <div class="column last js-hidden-pane" style='display:none'>
      <h3>Pull request list</h3>
      <dl class="keyboard-mappings">
        <dt>j</dt>
        <dd>Move selection down</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>k</dt>
        <dd>Move selection up</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>o <em>or</em> enter</dt>
        <dd>Open issue</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt><span class="platform-mac">⌘</span><span class="platform-other">ctrl</span> <em>+</em> enter</dt>
        <dd>Submit comment</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt><span class="platform-mac">⌘</span><span class="platform-other">ctrl</span> <em>+</em> shift p</dt>
        <dd>Preview comment</dd>
      </dl>
    </div><!-- /.columns.last -->

  </div><!-- /.columns.equacols -->

  <div class="js-hidden-pane" style='display:none'>
    <div class="rule"></div>

    <h3>Issues</h3>

    <div class="columns threecols">
      <div class="column first">
        <dl class="keyboard-mappings">
          <dt>j</dt>
          <dd>Move selection down</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>k</dt>
          <dd>Move selection up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>x</dt>
          <dd>Toggle selection</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>o <em>or</em> enter</dt>
          <dd>Open issue</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt><span class="platform-mac">⌘</span><span class="platform-other">ctrl</span> <em>+</em> enter</dt>
          <dd>Submit comment</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt><span class="platform-mac">⌘</span><span class="platform-other">ctrl</span> <em>+</em> shift p</dt>
          <dd>Preview comment</dd>
        </dl>
      </div><!-- /.column.first -->
      <div class="column last">
        <dl class="keyboard-mappings">
          <dt>c</dt>
          <dd>Create issue</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>l</dt>
          <dd>Create label</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>i</dt>
          <dd>Back to inbox</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>u</dt>
          <dd>Back to issues</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>/</dt>
          <dd>Focus issues search</dd>
        </dl>
      </div>
    </div>
  </div>

  <div class="js-hidden-pane" style='display:none'>
    <div class="rule"></div>

    <h3>Issues Dashboard</h3>

    <div class="columns threecols">
      <div class="column first">
        <dl class="keyboard-mappings">
          <dt>j</dt>
          <dd>Move selection down</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>k</dt>
          <dd>Move selection up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>o <em>or</em> enter</dt>
          <dd>Open issue</dd>
        </dl>
      </div><!-- /.column.first -->
    </div>
  </div>

  <div class="js-hidden-pane" style='display:none'>
    <div class="rule"></div>

    <h3>Network Graph</h3>
    <div class="columns equacols">
      <div class="column first">
        <dl class="keyboard-mappings">
          <dt><span class="badmono">←</span> <em>or</em> h</dt>
          <dd>Scroll left</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt><span class="badmono">→</span> <em>or</em> l</dt>
          <dd>Scroll right</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt><span class="badmono">↑</span> <em>or</em> k</dt>
          <dd>Scroll up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt><span class="badmono">↓</span> <em>or</em> j</dt>
          <dd>Scroll down</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>t</dt>
          <dd>Toggle visibility of head labels</dd>
        </dl>
      </div><!-- /.column.first -->
      <div class="column last">
        <dl class="keyboard-mappings">
          <dt>shift <span class="badmono">←</span> <em>or</em> shift h</dt>
          <dd>Scroll all the way left</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>shift <span class="badmono">→</span> <em>or</em> shift l</dt>
          <dd>Scroll all the way right</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>shift <span class="badmono">↑</span> <em>or</em> shift k</dt>
          <dd>Scroll all the way up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>shift <span class="badmono">↓</span> <em>or</em> shift j</dt>
          <dd>Scroll all the way down</dd>
        </dl>
      </div><!-- /.column.last -->
    </div>
  </div>

  <div class="js-hidden-pane" >
    <div class="rule"></div>
    <div class="columns threecols">
      <div class="column first js-hidden-pane" >
        <h3>Source Code Browsing</h3>
        <dl class="keyboard-mappings">
          <dt>t</dt>
          <dd>Activates the file finder</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>l</dt>
          <dd>Jump to line</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>w</dt>
          <dd>Switch branch/tag</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>y</dt>
          <dd>Expand URL to its canonical form</dd>
        </dl>
      </div>
    </div>
  </div>

  <div class="js-hidden-pane" style='display:none'>
    <div class="rule"></div>
    <div class="columns threecols">
      <div class="column first">
        <h3>Browsing Commits</h3>
        <dl class="keyboard-mappings">
          <dt><span class="platform-mac">⌘</span><span class="platform-other">ctrl</span> <em>+</em> enter</dt>
          <dd>Submit comment</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>escape</dt>
          <dd>Close form</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>p</dt>
          <dd>Parent commit</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>o</dt>
          <dd>Other parent commit</dd>
        </dl>
      </div>
    </div>
  </div>

  <div class="js-hidden-pane" style='display:none'>
    <div class="rule"></div>
    <h3>Notifications</h3>

    <div class="columns threecols">
      <div class="column first">
        <dl class="keyboard-mappings">
          <dt>j</dt>
          <dd>Move selection down</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>k</dt>
          <dd>Move selection up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>o <em>or</em> enter</dt>
          <dd>Open notification</dd>
        </dl>
      </div><!-- /.column.first -->

      <div class="column second">
        <dl class="keyboard-mappings">
          <dt>e <em>or</em> shift i <em>or</em> y</dt>
          <dd>Mark as read</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>shift m</dt>
          <dd>Mute thread</dd>
        </dl>
      </div><!-- /.column.first -->
    </div>
  </div>

</div>

    <div id="markdown-help" class="instapaper_ignore readability-extra">
  <h2>Markdown Cheat Sheet</h2>

  <div class="cheatsheet-content">

  <div class="mod">
    <div class="col">
      <h3>Format Text</h3>
      <p>Headers</p>
      <pre>
# This is an &lt;h1&gt; tag
## This is an &lt;h2&gt; tag
###### This is an &lt;h6&gt; tag</pre>
     <p>Text styles</p>
     <pre>
*This text will be italic*
_This will also be italic_
**This text will be bold**
__This will also be bold__

*You **can** combine them*
</pre>
    </div>
    <div class="col">
      <h3>Lists</h3>
      <p>Unordered</p>
      <pre>
* Item 1
* Item 2
  * Item 2a
  * Item 2b</pre>
     <p>Ordered</p>
     <pre>
1. Item 1
2. Item 2
3. Item 3
   * Item 3a
   * Item 3b</pre>
    </div>
    <div class="col">
      <h3>Miscellaneous</h3>
      <p>Images</p>
      <pre>
![GitHub Logo](/images/logo.png)
Format: ![Alt Text](url)
</pre>
     <p>Links</p>
     <pre>
http://github.com - automatic!
[GitHub](http://github.com)</pre>
<p>Blockquotes</p>
     <pre>
As Kanye West said:

> We're living the future so
> the present is our past.
</pre>
    </div>
  </div>
  <div class="rule"></div>

  <h3>Code Examples in Markdown</h3>
  <div class="col">
      <p>Syntax highlighting with <a href="http://github.github.com/github-flavored-markdown/" title="GitHub Flavored Markdown" target="_blank">GFM</a></p>
      <pre>
```javascript
function fancyAlert(arg) {
  if(arg) {
    $.facebox({div:'#foo'})
  }
}
```</pre>
    </div>
    <div class="col">
      <p>Or, indent your code 4 spaces</p>
      <pre>
Here is a Python code example
without syntax highlighting:

    def foo:
      if not bar:
        return true</pre>
    </div>
    <div class="col">
      <p>Inline code for comments</p>
      <pre>
I think you should use an
`&lt;addr&gt;` element here instead.</pre>
    </div>
  </div>

  </div>
</div>


    <div id="ajax-error-message" class="flash flash-error">
      <span class="mini-icon mini-icon-exclamation"></span>
      Something went wrong with that request. Please try again.
      <a href="#" class="mini-icon mini-icon-remove-close ajax-error-dismiss"></a>
    </div>

    
    
    <span id='server_response_time' data-time='0.07829' data-host='fe16'></span>
    
  </body>
</html>

