<?xml version="1.0" encoding="UTF-8"?>

<project>
  <modelVersion>4.0.0</modelVersion>

  <groupId>com.adacore</groupId>
  <artifactId>${ctx.config.library.language_name.lower}ls</artifactId>
  <version>0.1</version>

  <properties>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
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

  <build>
    <plugins>
      <plugin>
          <artifactId>maven-compiler-plugin</artifactId>
          <version>3.7.0</version>
          <configuration>
              <source>17</source>
              <target>17</target>
          </configuration>
      </plugin>

      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-shade-plugin</artifactId>
        <version>3.5.2</version>
        <executions>
          <execution>
            <phase>package</phase>
            <goals>
              <goal>shade</goal>
            </goals>
            <configuration>
              <filters>
                <filter>
                  <artifact>*:*</artifact>
                  <excludes>
                    <exclude>META-INF/*.SF</exclude>
                    <exclude>META-INF/*.DSA</exclude>
                    <exclude>META-INF/*.RSA</exclude>
                  </excludes>
                </filter>
              </filters>
            </configuration>
          </execution>
        </executions>
      </plugin>
    </plugins>
  </build>
</project>
