<?xml version="1.0" encoding="UTF-8"?>

<project>
  <modelVersion>4.0.0</modelVersion>

  <groupId>com.adacore</groupId>
  <artifactId>${ctx.config.library.language_name.lower}ls</artifactId>
  <version>0.1</version>

  <properties>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    <config.python>python</config.python>
    <config.skipNativeBuild>true</config.skipNativeBuild>
  </properties>

  <dependencies>
    <dependency>
      <groupId>org.eclipse.lsp4j</groupId>
      <artifactId>org.eclipse.lsp4j</artifactId>
      <version>0.22.0</version>
    </dependency>

    <dependency>
      <groupId>com.adacore</groupId>
      <artifactId>${ctx.lib_name.lower}</artifactId>
      <version>0.1</version>
    </dependency>

    <dependency>
      <groupId>com.adacore</groupId>
      <artifactId>langkit_support</artifactId>
      <version>0.1</version>
    </dependency>

    <dependency>
      <groupId>com.adacore</groupId>
      <artifactId>lklsp</artifactId>
      <version>0.1</version>
    </dependency>

    <dependency>
      <groupId>org.reflections</groupId>
      <artifactId>reflections</artifactId>
      <version>0.9.12</version>
    </dependency>
  </dependencies>

  <profiles>
    <profile>
      <id>native</id>
      <properties>
        <config.skipNativeBuild>false</config.skipNativeBuild>
      </properties>
    </profile>
  </profiles>

  <build>
    <plugins>
      <plugin>
        <artifactId>maven-compiler-plugin</artifactId>
        <version>3.11.0</version>
        <configuration>
          <release>24</release>
        </configuration>
      </plugin>

      <!--- Copy all dependencies into a "lib" file to easily expose a
            standalone JVM version of the language server -->
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-dependency-plugin</artifactId>
        <version>3.2.0</version>
        <executions>
          <execution>
            <id>copy-dependencies</id>
            <phase>package</phase>
            <goals>
              <goal>copy-dependencies</goal>
            </goals>
            <configuration>
              <%text>
              <outputDirectory>${project.build.directory}/lib</outputDirectory>
              </%text>
              <stripVersion>true</stripVersion>
              <includeScope>runtime</includeScope>
              <excludeArtifactIds>builtins_annotations</excludeArtifactIds>
            </configuration>
          </execution>
        </executions>
      </plugin>


      <plugin>
        <groupId>org.codehaus.mojo</groupId>
        <artifactId>exec-maven-plugin</artifactId>
        <version>3.0.0</version>
        <executions>
          <execution>
            <id>make_native</id>
            <phase>package</phase>
            <goals>
              <goal>exec</goal>
            </goals>
            <%text>
            <configuration>
              <skip>${config.skipNativeBuild}</skip>
              <executable>${config.python}</executable>
              <commandlineArgs>
                ${basedir}/make_native_image.py
                --class-path %classpath
              </commandlineArgs>
            </%text>
            </configuration>
          </execution>
        </executions>
      </plugin>
    </plugins>
  </build>
</project>
