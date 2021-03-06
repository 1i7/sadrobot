package unfiltered.jetty

import javax.servlet.Filter

import org.eclipse.jetty.server.handler.ContextHandlerCollection
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHolder}
import org.eclipse.jetty.util.resource.Resource

trait ContextAdder {
  def addToParent(parent: ContextHandlerCollection): Unit
  def filterAdder(filter: FilterAdder): ContextAdder
  def plan(filter: Filter) = filterAdder(FilterAdder(BasicFilterHolder(filter)))
  @deprecated("Use `plan(filter)`", "0.8.1")
  def filter(filter: Filter) = plan(filter)
  def resources(path: java.net.URL): ContextAdder
  def allowAliases(aliases: Boolean): ContextAdder
}

case class DefaultServletContextAdder(
  path: String,
  filterAdders: List[FilterAdder],
  resourcePath: Option[java.net.URL],
  aliases: Boolean = false
) extends ContextAdder {
  def addToParent(parent: ContextHandlerCollection) = {
    val ctx = new ServletContextHandler(parent, path, true, false)
    val holder = new ServletHolder(classOf[org.eclipse.jetty.servlet.DefaultServlet])
    holder.setName(CountedName.Servlet.name)
    ctx.addServlet(holder, "/")

    for (filterAdder <- filterAdders.reverseIterator)
      filterAdder.addToContext(ctx)

    for (path <- resourcePath)
      ctx.setBaseResource(Resource.newResource(path))

    ctx.setAliases(aliases)
  }

  def filterAdder(filter: FilterAdder) = copy(filterAdders = filter :: filterAdders)

  def resources(path: java.net.URL) = copy(resourcePath = Some(path))

  def allowAliases(aliases: Boolean) = copy(aliases = aliases)
}
