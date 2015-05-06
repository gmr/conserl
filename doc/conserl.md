

# Module conserl #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Consul client library.
Copyright (c) 2015, Gavin M. Roy

__Version:__ 0.1.0

__Authors:__ Gavin M. Roy ([`gavinmroy@gmail.com`](mailto:gavinmroy@gmail.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start the application.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Start the application.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop the application.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-0"></a>

### start/0 ###


<pre><code>
start() -&gt; {ok, [atom()]}
</code></pre>
<br />

Start the application
<a name="start-2"></a>

### start/2 ###


<pre><code>
start(Type::atom(), Args::term()) -&gt; {ok, [atom()]}
</code></pre>
<br />

Start the application
<a name="stop-0"></a>

### stop/0 ###


<pre><code>
stop() -&gt; ok | {error, term()}
</code></pre>
<br />

Stop the application
