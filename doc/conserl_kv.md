

# Module conserl_kv #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Consul KV API endpoints.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete the specified key from the Consul KV database.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Recursively delete all keys under <code>Key</code> as a prefix when <code>Recurse</code>
is <code>true</code>, otherwise delete the key matching the <code>Key</code> value.</td></tr><tr><td valign="top"><a href="#delete-3">delete/3</a></td><td>Delete the given <code>Key</code> using Check-and-Set operations specifying the
<code>ModifyIndex</code> using the <code>CAS</code> argument, returning <code>Result</code>.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Return <code>Result</code> value for the given key.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Return <code>Result</code> for the given <code>Key</code> and specified query args (<code>QArgs</code>)
such as <code>{"dc", "production"}</code>.</td></tr><tr><td valign="top"><a href="#get_all-1">get_all/1</a></td><td>Return the values for all keys with the supplied <code>Prefix</code>.</td></tr><tr><td valign="top"><a href="#get_all-2">get_all/2</a></td><td>Return the values for all keys with the supplied <code>Prefix</code> passing in
aditional query arguments (<code>QArgs</code>), such as <code>{"dc", "production"}</code>.</td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>List all keys under the given <code>Prefix</code>.</td></tr><tr><td valign="top"><a href="#keys-2">keys/2</a></td><td>List keys for the prefix.</td></tr><tr><td valign="top"><a href="#put-2">put/2</a></td><td>Store <code>Value</code> for <code>Key</code> returning <code>Result</code>.</td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td>Store <code>Value</code> while specifying <code>Flags</code> for <code>Key</code> returning <code>Result</code>.</td></tr><tr><td valign="top"><a href="#put-4">put/4</a></td><td>Store <code>Value</code> while specifying <code>Flags</code> for <code>Key</code>, using the
Check-and-Set operation, specifying the <code>ModifyIndex</code> value as <code>CAS</code>
returning <code>Result</code>.</td></tr><tr><td valign="top"><a href="#watch-1">watch/1</a></td><td>Blocking watch on the specified <code>Key</code>, returning <code>Result</code>.</td></tr><tr><td valign="top"><a href="#watch-2">watch/2</a></td><td>Asynchonous watch on the specified <code>Key</code>, calling the callback <code>Fun</code>
with the <code>Result</code>.</td></tr><tr><td valign="top"><a href="#watch-3">watch/3</a></td><td>Asynchonous watch on the specified key, calling the callback 'Fun'
with the results of the call as Result, passing in aditional query
arguments (<code>QArgs</code>), such as <code>{"dc", "production"}</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Key::list()) -&gt; ok | {error, list()}
</code></pre>
<br />


Delete the specified key from the Consul KV database.



*Example*:



```erlang

  ok = conserl_kv:delete("key").
```

<a name="delete-2"></a>

### delete/2 ###


<pre><code>
delete(Key::list(), Recurse::boolean()) -&gt; ok | {error, list()}
</code></pre>
<br />


Recursively delete all keys under `Key` as a prefix when `Recurse`
is `true`, otherwise delete the key matching the `Key` value.



*Example*:



```erlang

  ok = conserl_kv:delete("foo/bar/", true).
```

<a name="delete-3"></a>

### delete/3 ###


<pre><code>
delete(Key::list(), Recurse::boolean(), CAS::integer()) -&gt; ok | {error, list()}
</code></pre>
<br />


Delete the given `Key` using Check-and-Set operations specifying the
`ModifyIndex` using the `CAS` argument, returning `Result`. If `Recurse`
is set `true`, delete all keys under `Key` as a prefix.



*Example*:



```erlang

  Value = conserl_kv:get("maintenance"),
  ok = conserl_kv:delete("maintenance", false, maps:get(modify_index, Value).
```

<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, map()} | {error, Reason}</code></li></ul>

Return `Result` value for the given key.
<a name="get-2"></a>

### get/2 ###


<pre><code>
get(Key, QArgs) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>QArgs = list()</code></li><li><code>Result = {ok, map()} | {error, Reason}</code></li></ul>

Return `Result` for the given `Key` and specified query args (`QArgs`)
such as `{"dc", "production"}`.
<a name="get_all-1"></a>

### get_all/1 ###


<pre><code>
get_all(Prefix) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

Return the values for all keys with the supplied `Prefix`.
<a name="get_all-2"></a>

### get_all/2 ###


<pre><code>
get_all(Prefix, QArgs) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

Return the values for all keys with the supplied `Prefix` passing in
aditional query arguments (`QArgs`), such as `{"dc", "production"}`.
<a name="keys-1"></a>

### keys/1 ###


<pre><code>
keys(Prefix) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

List all keys under the given `Prefix`.
<a name="keys-2"></a>

### keys/2 ###


<pre><code>
keys(Prefix, QArgs) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>QArgs = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

List keys for the prefix. To add a separator for limiting the keys
returned, pass `{separator, Value}` in the `QArgs"" %% such
as ``{"dc", "production"}`.
<a name="put-2"></a>

### put/2 ###


<pre><code>
put(Key, Value) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>Value = list()</code></li><li><code>Result = boolean() | {error, Reason}</code></li></ul>

Store `Value` for `Key` returning `Result`.
<a name="put-3"></a>

### put/3 ###


<pre><code>
put(Key::list(), Value::list(), Flags::integer()) -&gt; boolean() | {error, list()}
</code></pre>
<br />

Store `Value` while specifying `Flags` for `Key` returning `Result`.
<a name="put-4"></a>

### put/4 ###


<pre><code>
put(Key, Value, Flags, CAS) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>Value = list()</code></li><li><code>Flags = integer()</code></li><li><code>CAS = integer()</code></li><li><code>Result = boolean() | {error, Reason}</code></li></ul>

Store `Value` while specifying `Flags` for `Key`, using the
Check-and-Set operation, specifying the `ModifyIndex` value as `CAS`
returning `Result`.
<a name="watch-1"></a>

### watch/1 ###


<pre><code>
watch(Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>Result = {ok, map()} | {error, Reason}</code></li></ul>

Blocking watch on the specified `Key`, returning `Result`.
<a name="watch-2"></a>

### watch/2 ###


<pre><code>
watch(Key, Fun) -&gt; {ok, <a href="#type-ref">ref()</a>}
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>Result = {<a href="#type-ref">ref()</a>, map()} | {<a href="#type-ref">ref()</a>, {error, Reason}}</code></li></ul>

Asynchonous watch on the specified `Key`, calling the callback `Fun`
with the `Result`.
<a name="watch-3"></a>

### watch/3 ###


<pre><code>
watch(Key, QArgs, Fun) -&gt; {ok, <a href="#type-ref">ref()</a>}
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>QArgs = list()</code></li><li><code>Result = {<a href="#type-ref">ref()</a>, map()} | {<a href="#type-ref">ref()</a>, {error, Reason}}</code></li></ul>

Asynchonous watch on the specified key, calling the callback 'Fun'
with the results of the call as Result, passing in aditional query
arguments (`QArgs`), such as `{"dc", "production"}`.
