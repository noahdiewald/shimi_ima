<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
	<title>
	epeli / Underscore.strings / source – Bitbucket
</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
	<meta name="description" content="" />
	<meta name="keywords" content="epeli,String,manipulation,extensions,for,the,Underscore.js,javascript,library.,source,sourcecode,underscore.strings-min.js@4c128e90f089" />

	<link rel="stylesheet" type="text/css" href="http://assets.bitbucket.org/dacf63410040/css/reset.css" />
	<link rel="stylesheet" type="text/css" href="http://assets.bitbucket.org/dacf63410040/css/layout.css" />
	<link rel="stylesheet" type="text/css" href="http://assets.bitbucket.org/dacf63410040/css/screen.css" />
	<link rel="stylesheet" type="text/css" href="http://assets.bitbucket.org/dacf63410040/css/print.css" media="print" />
	<link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="Bitbucket" />
	<link rel="icon" href="http://assets.bitbucket.org/dacf63410040/img/logo_new.png" type="image/png"/>

	<script type="text/javascript">var MEDIA_URL = "http://assets.bitbucket.org/dacf63410040/"</script>
	<script src="http://assets.bitbucket.org/dacf63410040/js/lib/bundle.020510May.js"></script>

	
	<link rel="stylesheet" href="http://assets.bitbucket.org/dacf63410040/css/highlight/trac.css" type="text/css" />

	
</head>

<body class="">



	<div id="header-wrap">
		<div id="header">
		<ul id="global-nav">
			<li><a class="home" href="http://www.atlassian.com">Atlassian Home</a></li>
			<li><a class="docs" href="http://confluence.atlassian.com/display/BITBUCKET">Documentation</a></li>
			<li><a class="support" href="/support">Support</a></li>
			<li><a class="blog" href="http://blog.bitbucket.org">Blog</a></li>
			<li><a class="forums" href="http://groups.google.com/group/bitbucket-users">Forums</a></li>
		</ul>
		<a href="/" id="logo">Bitbucket by Atlassian</a>

		<div id="main-nav" class="clearfix">
		
			<ul class="clearfix">
				<li><a href="/plans">Pricing &amp; Signup</a></li>
				<li><a href="/repo/all">Explore Bitbucket</a></li>
				<li><a href="/account/signin/">Log in</a></li>
				<li class="search-box">
					<form action="/repo/all/" method="get">
						<input id="search" type="text" name="name" />
					</form>
				</li>
			</ul>
		
		</div>
		</div>
	</div>

	
	
		
		
		
	

    



<div id="wrapper">

	<div id="content">
	
	
  





  <script src="http://assets.bitbucket.org/dacf63410040/js/lib/jquery.cookie.js"></script> <!--REMOVE WHEN NEWER BUNDLE THAN 030309Mar -->
  <script type="text/javascript">
    window._shard = 'bitbucket01 (ID 8)';

    $(document).ready(function () {
      var content = $('#content'), cookieOptions, date;
      date = new Date();
      date.setTime(date.getTime() + 365 * 24 * 60 * 60 * 1000);
      cookieOptions = { path: '/', expires: date };

      if ($.cookie('toggle_status') == 'hide') content.addClass('repo-desc-hidden');
      $('#toggle-repo-content').click(function (event) {
        event.preventDefault();
        content.toggleClass('repo-desc-hidden');
        $.cookie('toggle_status', $.cookie('toggle_status') == 'show' ? 'hide' : 'show', cookieOptions);
      });
    });
  </script>


<div id="tabs">
  <ul class="ui-tabs-nav">
    <li>
      <a href="/epeli/underscore.strings/overview"><span>Overview</span></a>
    </li>

    <li>
      <a href="/epeli/underscore.strings/downloads"><span>Downloads (0)</span></a>
    </li>

    

    <li class="ui-tabs-selected">
      
        <a href="/epeli/underscore.strings/src"><span>Source</span></a>
      
    </li>

    <li>
      <a href="/epeli/underscore.strings/changesets"><span>Changesets</span></a>
    </li>

    
      
        <li class="ui-tabs-nav-issues">
          <a href="/epeli/underscore.strings/wiki"><span>Wiki</span></a>
        </li>
      
    

    
      
        <li class="ui-tabs-nav-issues">
          <a href="/epeli/underscore.strings/issues?status=new&amp;status=open"><span>Issues (2) &raquo;</span></a>
          <ul>
            <li><a href="/epeli/underscore.strings/issues?status=new">New issues</a></li>
            <li><a href="/epeli/underscore.strings/issues?status=new&amp;status=open">Open issues</a></li>
            <li><a href="/epeli/underscore.strings/issues?status=resolved&amp;status=invalid&amp;status=duplicate">Closed issues</a></li>
            
            <li><a href="/epeli/underscore.strings/issues">All issues</a></li>
            <li><a href="/epeli/underscore.strings/issues/query">Advanced query</a></li>
            <li><a href="/epeli/underscore.strings/issues/new">Create new issue</a></li>
          </ul>
        </li>
      
    

    
      
    

    <li class="tabs-right tabs-far-right">
      <a href="/epeli/underscore.strings/descendants"><span>Forks/Queues (1)</span></a>
    </li>

    <li class="tabs-right">
      <a href="/epeli/underscore.strings/zealots"><span>Followers (4)</span></a>
    </li>
  </ul>
</div>

<div id="repo-menu">
  <div id="repo-menu-links">
    <ul>
      <li>
        <a href="/epeli/underscore.strings/rss" class="noborder repo-menu-rss" title="RSS Feed for Underscore.strings">RSS</a>
      </li>
      <li>
        <a href="/epeli/underscore.strings/atom" class="noborder repo-menu-atom" title="Atom Feed for Underscore.strings">Atom</a>
      </li>
      
        <li>
          <a href="/epeli/underscore.strings/pull" class="link-request-pull">
            pull request
          </a>
        </li>
      
      <li><a href="/epeli/underscore.strings/fork" class="link-fork">fork</a></li>
      
        <li><a href="/epeli/underscore.strings/hack" class="link-hack">patch queue</a></li>
      
      <li>
        
          <a rel="nofollow" href="/epeli/underscore.strings/follow" class="link-follow">follow</a>
        
      </li>
      
        <li><a class="link-download">get source &raquo;</a>

          <ul>
            
              
                <li><a rel="nofollow" href="/epeli/underscore.strings/get/4c128e90f089.zip" class="zip">zip</a></li>
                <li><a rel="nofollow" href="/epeli/underscore.strings/get/4c128e90f089.gz" class="compressed">gz</a></li>
                <li><a rel="nofollow" href="/epeli/underscore.strings/get/4c128e90f089.bz2" class="compressed">bz2</a></li>
              
            
          </ul>
        </li>
      
    </ul>
  </div>

  
    <div id="repo-menu-branches-tags">
      <ul>
        <li class="icon-branches">
          branches &raquo;
          
            <ul>
              
                <li><a href="/epeli/underscore.strings/src/4c128e90f089">default</a></li>
              
            </ul>
          
        </li>
        <li class="icon-tags">
          tags &raquo;

          <ul>
            
              <li><a href="/epeli/underscore.strings/src/4c128e90f089">tip</a></li>
            
          </ul>
        </li>
      </ul>
    </div>
  

  <div class="cb"></div>
</div>
<div id="repo-desc" class="layout-box">
  

  <div id="repo-menu-links-mini" class="right">
    <ul>
      <li>
        <a href="/epeli/underscore.strings/rss" class="noborder repo-menu-rss" title="RSS Feed for Underscore.strings"></a>
      </li>
      <li>
        <a href="/epeli/underscore.strings/atom" class="noborder repo-menu-atom" title="Atom Feed for Underscore.strings"></a>
      </li>
      
        <li>
          <a href="/epeli/underscore.strings/pull" class="tooltip noborder link-request-pull" title="Pull request"></a>
        </li>
      
      <li><a href="/epeli/underscore.strings/fork" class="tooltip noborder link-fork" title="Fork"></a></li>
      
        <li><a href="/epeli/underscore.strings/hack" class="tooltip noborder link-hack" title="Patch queue"></a></li>
      
      <li><a class="tooltip noborder link-download" title="Get source"></a>

        <ul>
          
            
              <li><a rel="nofollow" href="/epeli/underscore.strings/get/4c128e90f089.zip" class="zip">zip</a></li>
              <li><a rel="nofollow" href="/epeli/underscore.strings/get/4c128e90f089.gz" class="compressed">gz</a></li>
              <li><a rel="nofollow" href="/epeli/underscore.strings/get/4c128e90f089.bz2" class="compressed">bz2</a></li>
            
          
        </ul>
      </li>
    </ul>
  </div>

  <h3>
    <a href="/epeli">epeli</a> /
    <a href="/epeli/underscore.strings/src">Underscore.strings</a>
    
    
  </h3>

  
    
  

  <p class="repo-desc-description">String manipulation extensions for the Underscore.js javascript library. </p>

  <div id="repo-desc-cloneinfo">Clone this repository (size: 70.7 KB): <a href="http://bitbucket.org/epeli/underscore.strings" onclick="$('#clone-url-ssh').hide();$('#clone-url-https').toggle();return(false);"><small>HTTPS</small></a> / <a href="ssh://hg@bitbucket.org/epeli/underscore.strings" onclick="$('#clone-url-https').hide();$('#clone-url-ssh').toggle();return(false);"><small>SSH</small></a><br/>
    <pre id="clone-url-https">hg clone <a href="http://bitbucket.org/epeli/underscore.strings">http://bitbucket.org/epeli/underscore.strings</a></pre>

    <pre id="clone-url-ssh" style="display:none;">hg clone <a href="ssh://hg@bitbucket.org/epeli/underscore.strings">ssh://hg@bitbucket.org/epeli/underscore.strings</a></pre></div>

  <div class="cb"></div>
  <a href="#" id="toggle-repo-content"></a>

  

</div>



			




	<div id="source-summary" class="cset-merge">
		<dl class="relations">
			<dt>commit 22</dt>
			<dd>4c128e90f089</dd>
			<dt>parent 21</dt>
			<dd><a href="/epeli/underscore.strings/changeset/7da32200d139">7da32200d139</a></dd>
			<dt>parent 20</dt>
			<dd><a href="/epeli/underscore.strings/changeset/832fe0a045a6">832fe0a045a6</a></dd>
			<dt>branch</dt>
			<dd>default</dd>
			<dt>tags</dt>
			<dd>tip</dd>
		</dl>
		<p>merged from main</p>
                
<dl class="metadata">
  <dt>Who</dt>
  
  <dd><a href="/epeli">Esa-Matti Suuronen</a> / <a href="/epeli">epeli</a></dd>
  
  <dd><img alt="" src="http://www.gravatar.com/avatar/6398d275920f612b87785f8854abfc4a?d=identicon&s=32" class="avatar" /></dd>
  
  
  <dt>When</dt>
  <dd title="2010-07-07T14:26:53Z">4 months ago</dd>
</dl>

	</div>



<div id="source-path" class="layout-box">
	<a href="/epeli/underscore.strings/src">Underscore.strings</a> /
	
		
			
				underscore.strings-min.js
			
		
		
	
</div>


<div id="source-view" class="scroll-x">
	<table class="info-table">
		<tr>
			<th>r22:4c128e90f089</th>
			<th>5 loc</th>
			<th>2.2 KB</th>
			<th class="source-view-links">
				<a id="embed-link" href="#" onclick="makeEmbed('#embed-link', 'http://bitbucket.org/epeli/underscore.strings/src/4c128e90f089/underscore.strings-min.js?embed=t');">embed</a> /
				<a href='/epeli/underscore.strings/history/underscore.strings-min.js'>history</a> / 
				<a href='/epeli/underscore.strings/annotate/4c128e90f089/underscore.strings-min.js'>annotate</a> / 
				<a href='/epeli/underscore.strings/raw/4c128e90f089/underscore.strings-min.js'>raw</a> / 
				<form action="/epeli/underscore.strings/diff/underscore.strings-min.js" method="get" class="source-view-form">
					
					<input type="hidden" name="diff2" value="4c128e90f089"/>
						<select name="diff1" class="smaller">
							
								
									<option value="45cf7a503f0c">
										r14:45cf7a503f0c
									</option>
								
							
								
									<option value="40652c7d8ae4">
										r13:40652c7d8ae4
									</option>
								
							
								
									<option value="83240188d06d">
										r11:83240188d06d
									</option>
								
							
								
									<option value="96ebe328686e">
										r10:96ebe328686e
									</option>
								
							
								
									<option value="b5232dbc8b3d">
										r3:b5232dbc8b3d
									</option>
								
							
								
									<option value="b55f63b1a409">
										r0:b55f63b1a409
									</option>
								
							
								
									<option value="832fe0a045a6">
										r20:832fe0a045a6
									</option>
								
							
						</select>
						<input type="submit" value="diff" class="smaller"/>
					
				</form>
			</th>
		</tr>
	</table>
	
		
			<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><a href="#cl-1">1</a>
<a href="#cl-2">2</a>
<a href="#cl-3">3</a>
<a href="#cl-4">4</a>
<a href="#cl-5">5</a>
</pre></div></td><td class="code"><div class="highlight"><pre><a name="cl-1"></a><span class="p">(</span><span class="kd">function</span><span class="p">(){</span><span class="kd">function</span> <span class="nx">i</span><span class="p">(</span><span class="nx">b</span><span class="p">){</span><span class="k">if</span><span class="p">(</span><span class="nx">b</span><span class="p">)</span><span class="k">return</span> <span class="nx">f</span><span class="p">.</span><span class="nx">escapeRegExp</span><span class="p">(</span><span class="nx">b</span><span class="p">);</span><span class="k">return</span><span class="s2">&quot;\\s&quot;</span><span class="p">}</span><span class="kd">var</span> <span class="nx">j</span><span class="o">=</span><span class="nb">String</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">trim</span><span class="p">,</span><span class="nx">f</span><span class="p">;</span><span class="nx">f</span><span class="o">=</span><span class="k">this</span><span class="p">.</span><span class="nx">_s</span><span class="o">=</span><span class="p">{</span><span class="nx">capitalize</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">){</span><span class="k">return</span> <span class="nx">b</span><span class="p">.</span><span class="nx">charAt</span><span class="p">(</span><span class="mi">0</span><span class="p">).</span><span class="nx">toUpperCase</span><span class="p">()</span><span class="o">+</span><span class="nx">b</span><span class="p">.</span><span class="nx">substring</span><span class="p">(</span><span class="mi">1</span><span class="p">).</span><span class="nx">toLowerCase</span><span class="p">()},</span><span class="nx">join</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">){</span><span class="k">for</span><span class="p">(</span><span class="kd">var</span> <span class="nx">a</span><span class="o">=</span><span class="s2">&quot;&quot;</span><span class="p">,</span><span class="nx">d</span><span class="o">=</span><span class="mi">1</span><span class="p">;</span><span class="nx">d</span><span class="o">&lt;</span><span class="nx">arguments</span><span class="p">.</span><span class="nx">length</span><span class="p">;</span><span class="nx">d</span><span class="o">+=</span><span class="mi">1</span><span class="p">){</span><span class="nx">a</span><span class="o">+=</span><span class="nb">String</span><span class="p">(</span><span class="nx">arguments</span><span class="p">[</span><span class="nx">d</span><span class="p">]);</span><span class="k">if</span><span class="p">(</span><span class="nx">d</span><span class="o">!==</span><span class="nx">arguments</span><span class="p">.</span><span class="nx">length</span><span class="o">-</span><span class="mi">1</span><span class="p">)</span><span class="nx">a</span><span class="o">+=</span><span class="nb">String</span><span class="p">(</span><span class="nx">b</span><span class="p">)}</span><span class="k">return</span> <span class="nx">a</span><span class="p">},</span><span class="nx">escapeRegExp</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">){</span><span class="k">return</span> <span class="nx">b</span><span class="p">.</span><span class="nx">replace</span><span class="p">(</span><span class="err">/([-.*+?^</span><span class="cp">${</span><span class="p">}()</span><span class="o">|</span><span class="p">[</span>\<span class="p">]</span>\<span class="o">/</span>\\<span class="p">])</span><span class="o">/</span><span class="n">g</span><span class="p">,</span><span class="s">&quot;</span><span class="se">\\</span><span class="s">$1&quot;</span><span class="p">)</span><span class="cp">}</span><span class="err">,reverse:function(b){return Array.prototype.reverse.apply(b.split(&quot;&quot;)).join(&quot;&quot;)},contains:function(b,</span>
<a name="cl-2"></a><span class="nx">a</span><span class="p">){</span><span class="k">return</span> <span class="nx">b</span><span class="p">.</span><span class="nx">indexOf</span><span class="p">(</span><span class="nx">a</span><span class="p">)</span><span class="o">!==-</span><span class="mi">1</span><span class="p">},</span><span class="nx">clean</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">){</span><span class="k">return</span> <span class="nx">f</span><span class="p">.</span><span class="nx">strip</span><span class="p">(</span><span class="nx">b</span><span class="p">.</span><span class="nx">replace</span><span class="p">(</span><span class="sr">/\s+/g</span><span class="p">,</span><span class="s2">&quot; &quot;</span><span class="p">))},</span><span class="nx">trim</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">,</span><span class="nx">a</span><span class="p">){</span><span class="k">if</span><span class="p">(</span><span class="o">!</span><span class="nx">a</span><span class="o">&amp;&amp;</span><span class="nx">j</span><span class="p">)</span><span class="k">return</span> <span class="nx">j</span><span class="p">.</span><span class="nx">call</span><span class="p">(</span><span class="nx">b</span><span class="p">);</span><span class="nx">a</span><span class="o">=</span><span class="nx">i</span><span class="p">(</span><span class="nx">a</span><span class="p">);</span><span class="k">return</span> <span class="nx">b</span><span class="p">.</span><span class="nx">replace</span><span class="p">(</span><span class="k">new</span> <span class="nb">RegExp</span><span class="p">(</span><span class="s2">&quot;^[&quot;</span><span class="o">+</span><span class="nx">a</span><span class="o">+</span><span class="s2">&quot;]+|[&quot;</span><span class="o">+</span><span class="nx">a</span><span class="o">+</span><span class="s2">&quot;]+$&quot;</span><span class="p">,</span><span class="s2">&quot;g&quot;</span><span class="p">),</span><span class="s2">&quot;&quot;</span><span class="p">)},</span><span class="nx">ltrim</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">,</span><span class="nx">a</span><span class="p">){</span><span class="nx">a</span><span class="o">=</span><span class="nx">i</span><span class="p">(</span><span class="nx">a</span><span class="p">);</span><span class="k">return</span> <span class="nx">b</span><span class="p">.</span><span class="nx">replace</span><span class="p">(</span><span class="k">new</span> <span class="nb">RegExp</span><span class="p">(</span><span class="s2">&quot;^[&quot;</span><span class="o">+</span><span class="nx">a</span><span class="o">+</span><span class="s2">&quot;]+&quot;</span><span class="p">,</span><span class="s2">&quot;g&quot;</span><span class="p">),</span><span class="s2">&quot;&quot;</span><span class="p">)},</span><span class="nx">rtrim</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">,</span><span class="nx">a</span><span class="p">){</span><span class="nx">a</span><span class="o">=</span><span class="nx">i</span><span class="p">(</span><span class="nx">a</span><span class="p">);</span><span class="k">return</span> <span class="nx">b</span><span class="p">.</span><span class="nx">replace</span><span class="p">(</span><span class="k">new</span> <span class="nb">RegExp</span><span class="p">(</span><span class="s2">&quot;[&quot;</span><span class="o">+</span><span class="nx">a</span><span class="o">+</span><span class="s2">&quot;]+$&quot;</span><span class="p">,</span><span class="s2">&quot;g&quot;</span><span class="p">),</span><span class="s2">&quot;&quot;</span><span class="p">)},</span><span class="nx">startsWith</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">,</span><span class="nx">a</span><span class="p">){</span><span class="k">return</span> <span class="nx">b</span><span class="p">.</span><span class="nx">length</span><span class="o">&gt;=</span><span class="nx">a</span><span class="p">.</span><span class="nx">length</span><span class="o">&amp;&amp;</span><span class="nx">b</span><span class="p">.</span><span class="nx">substring</span><span class="p">(</span><span class="mi">0</span><span class="p">,</span><span class="nx">a</span><span class="p">.</span><span class="nx">length</span><span class="p">)</span><span class="o">===</span><span class="nx">a</span><span class="p">},</span><span class="nx">endsWith</span><span class="o">:</span><span class="kd">function</span><span class="p">(</span><span class="nx">b</span><span class="p">,</span><span class="nx">a</span><span class="p">){</span><span class="k">return</span> <span class="nx">b</span><span class="p">.</span><span class="nx">length</span><span class="o">&gt;=</span><span class="nx">a</span><span class="p">.</span><span class="nx">length</span><span class="o">&amp;&amp;</span><span class="nx">b</span><span class="p">.</span><span class="nx">substring</span><span class="p">(</span><span class="nx">b</span><span class="p">.</span><span class="nx">length</span><span class="o">-</span>
<a name="cl-3"></a><span class="nx">a</span><span class="p">.</span><span class="nx">length</span><span class="p">)</span><span class="o">===</span><span class="nx">a</span><span class="p">},</span><span class="nx">sprintf</span><span class="o">:</span><span class="kd">function</span><span class="p">(){</span><span class="k">for</span><span class="p">(</span><span class="kd">var</span> <span class="nx">b</span><span class="o">=</span><span class="mi">0</span><span class="p">,</span><span class="nx">a</span><span class="p">,</span><span class="nx">d</span><span class="o">=</span><span class="nx">arguments</span><span class="p">[</span><span class="nx">b</span><span class="o">++</span><span class="p">],</span><span class="nx">h</span><span class="o">=</span><span class="p">[],</span><span class="nx">c</span><span class="p">,</span><span class="nx">e</span><span class="p">,</span><span class="nx">g</span><span class="p">;</span><span class="nx">d</span><span class="p">;){</span><span class="k">if</span><span class="p">(</span><span class="nx">c</span><span class="o">=</span><span class="sr">/^[^\x25]+/</span><span class="p">.</span><span class="nx">exec</span><span class="p">(</span><span class="nx">d</span><span class="p">))</span><span class="nx">h</span><span class="p">.</span><span class="nx">push</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">0</span><span class="p">]);</span><span class="k">else</span> <span class="k">if</span><span class="p">(</span><span class="nx">c</span><span class="o">=</span><span class="sr">/^\x25{2}/</span><span class="p">.</span><span class="nx">exec</span><span class="p">(</span><span class="nx">d</span><span class="p">))</span><span class="nx">h</span><span class="p">.</span><span class="nx">push</span><span class="p">(</span><span class="s2">&quot;%&quot;</span><span class="p">);</span><span class="k">else</span> <span class="k">if</span><span class="p">(</span><span class="nx">c</span><span class="o">=</span><span class="sr">/^\x25(?:(\d+)\$)?(\+)?(0|&#39;[^$])?(-)?(\d+)?(?:\.(\d+))?([b-fosuxX])/</span><span class="p">.</span><span class="nx">exec</span><span class="p">(</span><span class="nx">d</span><span class="p">)){</span><span class="k">if</span><span class="p">((</span><span class="nx">a</span><span class="o">=</span><span class="nx">arguments</span><span class="p">[</span><span class="nx">c</span><span class="p">[</span><span class="mi">1</span><span class="p">]</span><span class="o">||</span><span class="nx">b</span><span class="o">++</span><span class="p">])</span><span class="o">==</span><span class="kc">null</span><span class="o">||</span><span class="nx">a</span><span class="o">==</span><span class="kc">undefined</span><span class="p">)</span><span class="k">throw</span><span class="s2">&quot;Too few arguments.&quot;</span><span class="p">;</span><span class="k">if</span><span class="p">(</span><span class="sr">/[^s]/</span><span class="p">.</span><span class="nx">test</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">7</span><span class="p">])</span><span class="o">&amp;&amp;</span><span class="k">typeof</span> <span class="nx">a</span><span class="o">!=</span><span class="s2">&quot;number&quot;</span><span class="p">)</span><span class="k">throw</span><span class="s2">&quot;Expecting number but found &quot;</span><span class="o">+</span><span class="k">typeof</span> <span class="nx">a</span><span class="p">;</span><span class="k">switch</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">7</span><span class="p">]){</span><span class="k">case</span> <span class="s2">&quot;b&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="nx">a</span><span class="p">.</span><span class="nx">toString</span><span class="p">(</span><span class="mi">2</span><span class="p">);</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;c&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="nb">String</span><span class="p">.</span><span class="nx">fromCharCode</span><span class="p">(</span><span class="nx">a</span><span class="p">);</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;d&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span>
<a name="cl-4"></a><span class="nb">parseInt</span><span class="p">(</span><span class="nx">a</span><span class="p">);</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;e&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="nx">c</span><span class="p">[</span><span class="mi">6</span><span class="p">]</span><span class="o">?</span><span class="nx">a</span><span class="p">.</span><span class="nx">toExponential</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">6</span><span class="p">])</span><span class="o">:</span><span class="nx">a</span><span class="p">.</span><span class="nx">toExponential</span><span class="p">();</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;f&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="nx">c</span><span class="p">[</span><span class="mi">6</span><span class="p">]</span><span class="o">?</span><span class="nb">parseFloat</span><span class="p">(</span><span class="nx">a</span><span class="p">).</span><span class="nx">toFixed</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">6</span><span class="p">])</span><span class="o">:</span><span class="nb">parseFloat</span><span class="p">(</span><span class="nx">a</span><span class="p">);</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;o&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="nx">a</span><span class="p">.</span><span class="nx">toString</span><span class="p">(</span><span class="mi">8</span><span class="p">);</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;s&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="p">(</span><span class="nx">a</span><span class="o">=</span><span class="nb">String</span><span class="p">(</span><span class="nx">a</span><span class="p">))</span><span class="o">&amp;&amp;</span><span class="nx">c</span><span class="p">[</span><span class="mi">6</span><span class="p">]</span><span class="o">?</span><span class="nx">a</span><span class="p">.</span><span class="nx">substring</span><span class="p">(</span><span class="mi">0</span><span class="p">,</span><span class="nx">c</span><span class="p">[</span><span class="mi">6</span><span class="p">])</span><span class="o">:</span><span class="nx">a</span><span class="p">;</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;u&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="nb">Math</span><span class="p">.</span><span class="nx">abs</span><span class="p">(</span><span class="nx">a</span><span class="p">);</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;x&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="nx">a</span><span class="p">.</span><span class="nx">toString</span><span class="p">(</span><span class="mi">16</span><span class="p">);</span><span class="k">break</span><span class="p">;</span><span class="k">case</span> <span class="s2">&quot;X&quot;</span><span class="o">:</span><span class="nx">a</span><span class="o">=</span><span class="nx">a</span><span class="p">.</span><span class="nx">toString</span><span class="p">(</span><span class="mi">16</span><span class="p">).</span><span class="nx">toUpperCase</span><span class="p">();</span><span class="k">break</span><span class="p">}</span><span class="nx">a</span><span class="o">=</span><span class="sr">/[def]/</span><span class="p">.</span><span class="nx">test</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">7</span><span class="p">])</span><span class="o">&amp;&amp;</span><span class="nx">c</span><span class="p">[</span><span class="mi">2</span><span class="p">]</span><span class="o">&amp;&amp;</span><span class="nx">a</span><span class="o">&gt;=</span><span class="mi">0</span><span class="o">?</span><span class="s2">&quot;+&quot;</span><span class="o">+</span><span class="nx">a</span><span class="o">:</span><span class="nx">a</span><span class="p">;</span><span class="nx">e</span><span class="o">=</span><span class="nx">c</span><span class="p">[</span><span class="mi">3</span><span class="p">]</span><span class="o">?</span><span class="nx">c</span><span class="p">[</span><span class="mi">3</span><span class="p">]</span><span class="o">==</span><span class="s2">&quot;0&quot;</span><span class="o">?</span><span class="s2">&quot;0&quot;</span><span class="o">:</span><span class="nx">c</span><span class="p">[</span><span class="mi">3</span><span class="p">].</span><span class="nx">charAt</span><span class="p">(</span><span class="mi">1</span><span class="p">)</span><span class="o">:</span><span class="s2">&quot; &quot;</span><span class="p">;</span><span class="nx">g</span><span class="o">=</span><span class="nx">c</span><span class="p">[</span><span class="mi">5</span><span class="p">]</span><span class="o">-</span><span class="nb">String</span><span class="p">(</span><span class="nx">a</span><span class="p">).</span><span class="nx">length</span><span class="o">-</span><span class="mi">0</span><span class="p">;</span><span class="k">if</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">5</span><span class="p">]){</span><span class="nx">g</span><span class="o">=</span><span class="nx">g</span><span class="p">;</span><span class="k">for</span><span class="p">(</span><span class="kd">var</span> <span class="nx">k</span><span class="o">=</span><span class="p">[];</span><span class="nx">g</span><span class="o">&gt;</span><span class="mi">0</span><span class="p">;</span><span class="nx">k</span><span class="p">[</span><span class="o">--</span><span class="nx">g</span><span class="p">]</span><span class="o">=</span><span class="nx">e</span><span class="p">);</span><span class="nx">e</span><span class="o">=</span><span class="nx">k</span><span class="p">.</span><span class="nx">join</span><span class="p">(</span><span class="s2">&quot;&quot;</span><span class="p">)}</span><span class="k">else</span> <span class="nx">e</span><span class="o">=</span>
<a name="cl-5"></a><span class="s2">&quot;&quot;</span><span class="p">;</span><span class="nx">e</span><span class="o">=</span><span class="nx">e</span><span class="p">;</span><span class="nx">h</span><span class="p">.</span><span class="nx">push</span><span class="p">(</span><span class="s2">&quot;&quot;</span><span class="o">+</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">4</span><span class="p">]</span><span class="o">?</span><span class="nx">a</span><span class="o">+</span><span class="nx">e</span><span class="o">:</span><span class="nx">e</span><span class="o">+</span><span class="nx">a</span><span class="p">))}</span><span class="k">else</span> <span class="k">throw</span><span class="s2">&quot;Huh ?!&quot;</span><span class="p">;</span><span class="nx">d</span><span class="o">=</span><span class="nx">d</span><span class="p">.</span><span class="nx">substring</span><span class="p">(</span><span class="nx">c</span><span class="p">[</span><span class="mi">0</span><span class="p">].</span><span class="nx">length</span><span class="p">)}</span><span class="k">return</span> <span class="nx">h</span><span class="p">.</span><span class="nx">join</span><span class="p">(</span><span class="s2">&quot;&quot;</span><span class="p">)}};</span><span class="k">this</span><span class="p">.</span><span class="nx">_s</span><span class="p">.</span><span class="nx">strip</span><span class="o">=</span><span class="nx">f</span><span class="p">.</span><span class="nx">trim</span><span class="p">;</span><span class="k">this</span><span class="p">.</span><span class="nx">_s</span><span class="p">.</span><span class="nx">lstrip</span><span class="o">=</span><span class="nx">f</span><span class="p">.</span><span class="nx">ltrim</span><span class="p">;</span><span class="k">this</span><span class="p">.</span><span class="nx">_s</span><span class="p">.</span><span class="nx">rstrip</span><span class="o">=</span><span class="nx">f</span><span class="p">.</span><span class="nx">rtrim</span><span class="p">;</span><span class="k">this</span><span class="p">.</span><span class="nx">_</span><span class="o">&amp;&amp;</span><span class="k">this</span><span class="p">.</span><span class="nx">_</span><span class="p">.</span><span class="nx">mixin</span><span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">_s</span><span class="p">)})();</span>
</pre></div>
</td></tr></table>
		
	
</div>



	</div>


	<div id="footer" class="clearfix">
		<ul class="footer-nav">
			<li>Copyright &copy; 2010 <a href="http://atlassian.com">Atlassian</a><span class="pipe">|</span></li>
			<li><a href="http://www.atlassian.com/hosted/terms.jsp">Terms of Service</a><span class="pipe">|</span></li>
			<li><a href="http://www.atlassian.com/about/privacy.jsp">Privacy</a><span class="pipe">|</span></li>
                        <li><a href="http://bitbucket.org/site/master/issues/new">Report a Bug</a><span class="pipe">|</span></li>
			<li><a href="http://status.bitbucket.org/">Server Status</a></li>
		</ul>
		<ul class="social-nav">
			<li class="blog"><a href="http://blog.bitbucket.org">Bitbucket Blog</a></li>
			<li class="twitter"><a href="http://www.twitter.com/bitbucket">Twitter</a></li>
		</ul>
	</div>
    <div id="footer2" class="clearfix">
        We run 
            <a href="http://www.djangoproject.com/">Django 1.2.3</a> / 
            <a href="http://bitbucket.org/jespern/django-piston/">Piston 0.2.3rc1</a> / 
            <a href="http://www.selenic.com/mercurial/">Hg 1.6</a> / 
            <a href="http://www.python.org">Python 2.7.0</a> /
            r4099:dacf63410040 | bitbucket01
        
    </div>

</div>


	<script src="http://assets.bitbucket.org/dacf63410040/js/lib/global.js" id="globaljs"></script>





<!-- <script type="text/javascript"> Cufon.now(); </script> -->

	<script type="text/javascript">
		var _gaq = _gaq || [];
		_gaq.push(['_setAccount', 'UA-2456069-3'], ['_trackPageview']);
	
		/* User specified tracking. */
		_gaq.push(['repo._setAccount', 'UA-10526874-7'], ['repo._trackPageview']);
	
		_gaq.push(['atl._setAccount', 'UA-6032469-33'], ['atl._trackPageview']);
		(function () {
		    var ga = document.createElement('script');
		    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
		    ga.setAttribute('async', 'true');
		    document.documentElement.firstChild.appendChild(ga);
		}());
	</script>

</body>
</html>
