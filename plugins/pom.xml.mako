<?xml version="1.0" encoding="UTF-8"?> <!-- -*- mode: xml -*- -->
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.rabbitmq.streams.plugins</groupId>
  <artifactId>streams-plugins</artifactId>
  <packaging>pom</packaging>
  <version>1.0-SNAPSHOT</version>
  <name>Streams Plugins</name>
  <url>http://www.rabbitmq.com/</url>

  <repositories>
    <repository>
      <id>streams</id>
      <name>RabbitMQ Streams</name>
      <url>http://maven.feedshub.lshift.net</url>
    </repository>
    <repository>
      <id>scala-tools.org</id>
      <name>Scala-Tools Maven2 Repository</name>
      <url>http://scala-tools.org/repo-releases</url>
    </repository>
  </repositories>

  <pluginRepositories>
    <pluginRepository>
      <id>scala-tools.org</id>
      <name>Scala-Tools Maven2 Repository</name>
      <url>http://scala-tools.org/repo-releases</url>
    </pluginRepository>
  </pluginRepositories>
  <modules>
  % for module in modules:
    <module>${module}</module>
  % endfor
  </modules>

  <build>
    <extensions>
      <extension>
        <groupId>org.apache.maven.wagon</groupId>
        <artifactId>wagon-ssh-external</artifactId>
        <version>1.0-alpha-5</version>
      </extension>
    </extensions>

    <plugins>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-compiler-plugin</artifactId>
        <configuration>
          <source>1.5</source>
          <target>1.5</target>
        </configuration>
      </plugin>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-javadoc-plugin</artifactId>
        <version>2.6</version>
        <configuration>
          <use>false</use>
        </configuration>
      </plugin>
    </plugins>
  </build>

  <distributionManagement>
    <repository>
      <id>internal.repo</id>
      <url>scpexe://feedshub/srv/www/maven</url>
    </repository>
  </distributionManagement>

  <dependencies>
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>4.4</version>
      <scope>test</scope>
    </dependency>
    <dependency>
      <groupId>org.mockito</groupId>
      <artifactId>mockito-all</artifactId>
      <version>1.7</version>
    </dependency>
    <dependency>
      <groupId>net.sf.json-lib</groupId>
      <artifactId>json-lib</artifactId>
      <version>2.2.3</version>
      <classifier>jdk15</classifier>
    </dependency>
  </dependencies>
</project>
