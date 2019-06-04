scala.quoted.Exprs$
# class TastyExpr

<pre><code class="language-scala" >final class TastyExpr[T] extends Expr[<a href="./TastyExpr.md#T">T</a>]</pre></code>
An Expr backed by a pickled TASTY tree

## Constructors:
<pre><code class="language-scala" >TastyExpr(tasty: Pickled, args: Seq[Any])</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### run
<pre><code class="language-scala" >final def run(toolbox: <a href="../Toolbox.md">Toolbox</a>): <a href="../Expr.md#T">T</a></pre></code>
Evaluate the contents of this expression and return the result.
May throw a FreeVariableError on expressions that came from a macro.

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >override def toString: String</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### args
<pre><code class="language-scala" >val args: Seq[Any]</pre></code>

### tasty
<pre><code class="language-scala" >val tasty: Pickled</pre></code>
