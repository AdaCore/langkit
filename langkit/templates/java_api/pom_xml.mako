<%
    if ctx.version:
        version = ctx.version
        if ctx.build_date:
            version += "-" + ctx.build_date
    else:
        version = "0.1"
%>

<project>

    <modelVersion>4.0.0</modelVersion>

    <groupId>com.adacore</groupId>
    <artifactId>${ctx.lib_name.lower}</artifactId>
    <version>${version}</version>

    <properties>
        <maven.compiler.source>1.8</maven.compiler.source>
        <maven.compiler.target>1.8</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <build>
        <plugins>

            <plugin>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.7.0</version>
                <configuration>
                    <compilerArgs>
                        <arg>-Xlint:all</arg>
                        <arg>-h</arg>
                        <arg>${"${project.basedir}${file.separator}"}jni</arg>
                    </compilerArgs>
                    <source>8</source>
                    <target>8</target>
                </configuration>
            </plugin>

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

    <dependencies>
        <dependency>
            <groupId>org.graalvm.truffle</groupId>
            <artifactId>truffle-api</artifactId>
            <version>22.3.1</version>
        </dependency>
    </dependencies>

</project>
