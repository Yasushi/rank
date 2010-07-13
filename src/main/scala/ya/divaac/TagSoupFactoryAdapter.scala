package ya.divaac

import scala.xml.parsing.NoBindingFactoryAdapter
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl 

class TagSoupFactoryAdapter extends NoBindingFactoryAdapter
{
  private val parserFactory = new SAXFactoryImpl
  parserFactory.setNamespaceAware(true)

  override def parser = parserFactory.newSAXParser()
}
