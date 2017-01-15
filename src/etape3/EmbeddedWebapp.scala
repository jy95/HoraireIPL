package etape3

object EmbeddedWebapp {

  import org.eclipse.jetty.server.Server
  import org.eclipse.jetty.server.ServerConnector
  import org.eclipse.jetty.webapp.WebAppContext
  import org.eclipse.jetty.servlet.ServletHolder;
 
  
  def main(args: Array[String]): Unit = {
    val port = 8080;
    val contextPath = "/";
    val server = new Server()
    val connector = new ServerConnector(server)
    connector.setPort(port)
    server.addConnector(connector)

    val context = new WebAppContext()
    context.setResourceBase("web")
    context.setContextPath(contextPath)
    context.addServlet(new ServletHolder(TimeTableServlet), "/solver/*");
    context.setWelcomeFiles( Array("index.html") );
    server.setHandler(context)
    server.start();
  }

}