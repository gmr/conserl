

# Module conserl_catalog #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Consul Catalog API endpoints.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#nodes-0">nodes/0</a></td><td>Return nodes as a list.</td></tr><tr><td valign="top"><a href="#nodes-1">nodes/1</a></td><td>Return nodes as a list.</td></tr><tr><td valign="top"><a href="#services-0">services/0</a></td><td>Return services as a list.</td></tr><tr><td valign="top"><a href="#services-1">services/1</a></td><td>Return services as a proplist of service name a list of nodes.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="nodes-0"></a>

### nodes/0 ###


<pre><code>
nodes() -&gt; list()
</code></pre>
<br />

Return nodes as a list
<a name="nodes-1"></a>

### nodes/1 ###


<pre><code>
nodes(DC) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>DC = list()</code></li></ul>

Return nodes as a list
<a name="services-0"></a>

### services/0 ###


<pre><code>
services() -&gt; list()
</code></pre>
<br />

Return services as a list
<a name="services-1"></a>

### services/1 ###


<pre><code>
services(DC) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>DC = list()</code></li></ul>

Return services as a proplist of service name a list of nodes
