

# Module conserl_kv #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Consul KV API endpoints.
Copyright (c) 2015, Gavin M. Roy

__Authors:__ Gavin M. Roy ([`gavinmroy@gmail.com`](mailto:gavinmroy@gmail.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Return a map() value for the given key.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_all-1">get_all/1</a></td><td>Return the values for all keys with the supplied prefix.</td></tr><tr><td valign="top"><a href="#get_all-2">get_all/2</a></td><td>Return the values for all keys with the supplied prefix passing in
aditional query arguments, such as 'dc'.</td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>List all keys under the given prefix.</td></tr><tr><td valign="top"><a href="#keys-2">keys/2</a></td><td>List keys for the prefix.</td></tr><tr><td valign="top"><a href="#watch-1">watch/1</a></td><td>Blocking watch on the specified key.</td></tr><tr><td valign="top"><a href="#watch-2">watch/2</a></td><td>Asynchonous watch on the specified key, calling the callback 'Fun'
with the results of the call as Result.</td></tr><tr><td valign="top"><a href="#watch-3">watch/3</a></td><td>Asynchonous watch on the specified key, calling the callback 'Fun'
with the results of the call as Result, passing in aditional query
arguments, such as 'dc'.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Key) -&gt; map()
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

Return a map() value for the given key.
<a name="get-2"></a>

### get/2 ###

`get(Key, QArgs) -> any()`


<a name="get_all-1"></a>

### get_all/1 ###


<pre><code>
get_all(Prefix) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

Return the values for all keys with the supplied prefix.
<a name="get_all-2"></a>

### get_all/2 ###


<pre><code>
get_all(Prefix, QArgs) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

Return the values for all keys with the supplied prefix passing in
aditional query arguments, such as 'dc'.
<a name="keys-1"></a>

### keys/1 ###


<pre><code>
keys(Prefix) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

List all keys under the given prefix
<a name="keys-2"></a>

### keys/2 ###


<pre><code>
keys(Prefix, QArgs) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>QArgs = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

List keys for the prefix. To add a separator for limiting the keys
returned, pass '{separator, Value}' in the QArgs.
<a name="watch-1"></a>

### watch/1 ###


<pre><code>
watch(Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, map()} | {error, Reason}</code></li></ul>

Blocking watch on the specified key
<a name="watch-2"></a>

### watch/2 ###


<pre><code>
watch(Key, Fun) -&gt; {ok, <a href="#type-ref">ref()</a>}
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {<a href="#type-ref">ref()</a>, map()} | {<a href="#type-ref">ref()</a>, {error, Reason}}</code></li></ul>

Asynchonous watch on the specified key, calling the callback 'Fun'
with the results of the call as Result.
<a name="watch-3"></a>

### watch/3 ###


<pre><code>
watch(Key, QArgs, Fun) -&gt; {ok, <a href="#type-ref">ref()</a>}
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>QArgs = list()</code></li><li><code>Result = {<a href="#type-ref">ref()</a>, map()} | {<a href="#type-ref">ref()</a>, {error, Reason}}</code></li></ul>

Asynchonous watch on the specified key, calling the callback 'Fun'
with the results of the call as Result, passing in aditional query
arguments, such as 'dc'.
