<%
    if ctx.config.library.version == "undefined":
        version = "0.1"
    else:
        version = ctx.config.library.version
        if ctx.config.library.build_date != "undefined":
            version += "-" + ctx.config.library.build_date
%>

<project>

    <modelVersion>4.0.0</modelVersion>

    <groupId>com.adacore</groupId>
    <artifactId>${ctx.lib_name.lower}</artifactId>
    <version>${version}</version>

    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <build>
        <plugins>

            <plugin>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.11.0</version>
                <configuration>
                    <compilerArgs>
                        <arg>-Xlint:all</arg>
                        <arg>-h</arg>
                        <arg>${"${project.basedir}${file.separator}"}jni</arg>
                    </compilerArgs>
                    <release>24</release>
                </configuration>
            </plugin>

        </plugins>
    </build>

    <dependencies>
        <dependency>
            <groupId>org.graalvm.truffle</groupId>
            <artifactId>truffle-api</artifactId>
            <version>24.2.1</version>
        </dependency>

        <dependency>
            <groupId>com.adacore</groupId>
            <artifactId>langkit_support</artifactId>
            <version>0.1</version>
        </dependency>
    </dependencies>

    <profiles>
        <profile>
            <id>with-jni-stubs</id>

            <activation>
                <file>
                    <exists>Makefile</exists>
                </file>
            </activation>

            <build>
                <plugins>
                    <plugin>
                        <artifactId>exec-maven-plugin</artifactId>
                        <groupId>org.codehaus.mojo</groupId>
                        <version>3.0.0</version>
                        <executions>

                            <execution>
                                <id>make_all</id>
                                <phase>package</phase>
                                <goals>
                                    <goal>exec</goal>
                                </goals>
                                <configuration>
                                    <executable>make</executable>
                                    <arguments>all</arguments>
                                </configuration>
                            </execution>

                            <execution>
                                <id>make_clean</id>
                                <phase>clean</phase>
                                <goals>
                                    <goal>exec</goal>
                                </goals>
                                <configuration>
                                    <executable>make</executable>
                                    <arguments>clean</arguments>
                                </configuration>
                            </execution>

                        </executions>
                    </plugin>
                </plugins>
            </build>
        </profile>

        <profile>
            <id>with-shade</id>

            <activation>
                <property>
                    <name>skipShade</name>
                    <value>!true</value>
                </property>
            </activation>

            <build>
                <plugins>
                    <plugin>
                        <groupId>org.apache.maven.plugins</groupId>
                        <artifactId>maven-shade-plugin</artifactId>
                        <version>3.1.1</version>
                        <executions>
                            <execution>
                                <phase>package</phase>
                                <goals>
                                    <goal>shade</goal>
                                </goals>
                                <configuration>
                                    <finalName>
                                        ${ctx.lib_name.lower}
                                    </finalName>
                                </configuration>
                            </execution>
                        </executions>
                    </plugin>
                </plugins>
            </build>

        </profile>
    </profiles>

</project>
