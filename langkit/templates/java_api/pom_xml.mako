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
        <finalName>${ctx.lib_name.lower}</finalName>

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
                            <outputDirectory>
                                ${project.build.directory}/lib
                            </outputDirectory>
                            </%text>
                            <stripVersion>true</stripVersion>
                            <includeScope>runtime</includeScope>
                        </configuration>
                    </execution>
                </executions>
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
    </profiles>

</project>
