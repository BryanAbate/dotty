scala.tasty.reflect.TypeOrBoundsOps.Type$
# object TermRef

<pre><code class="language-scala" >final object TermRef extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(qual: TypeOrBounds, name: String)(implicit ctx: Context): TermRef</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds)]</pre></code>

