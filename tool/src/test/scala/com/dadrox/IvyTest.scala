package com.dadrox

import org.junit.Test
import org.apache.ivy.core.settings.IvySettings
import java.io.File
import org.apache.ivy.Ivy
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.apache.ivy.core.module.id.ModuleId
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.module.descriptor.DefaultModuleDescriptor
import org.apache.ivy.core.module.descriptor.Configuration
import org.apache.ivy.core.module.descriptor.DefaultDependencyDescriptor
import org.apache.ivy.core.module.descriptor.DependencyDescriptor
import java.util.Date

class IvyTest {

  @Test
  def moduleDefinition {
    val ivy = Ivy.newInstance
    configureIvy(ivy)
    val module = buildModuleDescriptor
    ivy.resolve(dumpIvy(module))
  }

  def dumpIvy(moduleDescriptor: ModuleDescriptor): File = {
    //This suck but it is the only way to get it to work for now
    val tmpFile = File.createTempFile("ivy", ".xml", new File("/tmp"))
    moduleDescriptor.toIvyFile(tmpFile)
    tmpFile
  }
    
  def configureIvy(ivy: Ivy) {
    val ivySettingsXmlFile = new File("tool/src/test/resources/ivysettings.xml");
    ivy.configure(ivySettingsXmlFile);
  }

  def buildModuleDescriptor: ModuleDescriptor = {
    val aDependency = createDependency("commons-httpclient", "commons-httpclient", "3.0", "release")
    val moduleDescriptor = createModule("org.module", "someModule", "0.1", "release", List(aDependency), List(new Configuration("default")) )
    moduleDescriptor.setDefaultConf("default")
    moduleDescriptor
  }

  def createModule(organization: String, artifactName: String, version: String, status: String, dependencies: List[DependencyDescriptor], configurations: List[Configuration], publicationDate: Option[Date] = None): DefaultModuleDescriptor = {
    val moduleDescriptor = createModuleDescriptor(organization, artifactName, version, status, publicationDate)
    configurations.foreach(moduleDescriptor.addConfiguration(_))
    dependencies.foreach(moduleDescriptor.addDependency(_))
    moduleDescriptor
  }
  
  def createDependency(organization: String, artifactName: String, version: String, status: String, publicationDate: Option[Date] = None,
                               force: Boolean = false, changing: Boolean = false, transitive: Boolean = true): DependencyDescriptor = {

    val depModule = createModuleDescriptor(organization, artifactName, version, status, publicationDate)

    depModule.addConfiguration(new Configuration("default"))

    new DefaultDependencyDescriptor(depModule, depModule.getModuleRevisionId, force, changing, transitive)
  }

  def createModuleDescriptor(organization: String, artifactName: String, version: String, status: String, publicationDate: Option[Date] = None): DefaultModuleDescriptor = {
    val depRevId = new ModuleRevisionId(new ModuleId(organization, artifactName), version)
    publicationDate match {
      case Some(publicationDate) => new DefaultModuleDescriptor(depRevId, status, publicationDate)
      case None                  => new DefaultModuleDescriptor(depRevId, status, null)
    }
  }
}