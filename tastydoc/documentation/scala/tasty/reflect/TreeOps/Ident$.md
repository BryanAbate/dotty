scala.tasty.reflect.TreeOps
# object Ident

<pre><code class="language-scala" >final object Ident extends Serializable</pre></code>
Scala term identifier

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(tmref: TermRef)(implicit ctx: Context): Term</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Tree)(name: String)(implicit ctx: Context): Ident</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[String]</pre></code>
Matches a term identifier and returns its name

