scala.tasty.reflect.PatternOps.Pattern$
# object Value

<pre><code class="language-scala" >final object Value extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(tpt: Term)(implicit ctx: Context): Value</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Value)(tpt: Term)(implicit ctx: Context): Value</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(pattern: Pattern)(implicit ctx: Context): Option[Term]</pre></code>

