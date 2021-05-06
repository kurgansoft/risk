# Risk

The classic strategy game Risk with the counties of Hungary. Implemented with the [gbge](https://github.com/kurgansoft/gbge) engine.

# Building & deploying

Before you try to build or deploy, make sure that

1) The gbge artifacts are available from your local ivy cache. [Instructions](https://github.com/kurgansoft/gbge#readme)
2) [Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html#_installation) is installed.

Once the application is deployed:
* Clients can join the game from their favourite browser, visiting: http://localhost:8080
* On your big screen you can display the board; visit: http://localhost:8080/s/

# Deploying by executing the jar file:

If you are on Linux, you can create a jar file by executing the create_jar.sh file from the root folder.
The resulting jar file will be in the out/Risk folder.
From there you can simply run the jar file. Checkout the [available VM OPTIONS](https://github.com/kurgansoft/gbge/blob/master/VM_OPTIONS.md).

So launching the app with below command would bind the IP address to 192.168.0.5, and the port would be 8765.

```
java -DhostAddress="192.168.0.5" -Dport=8765 -jar risk.jar
```

# Building manually

1) Building the UI:
    * Navigate to the root folder of this repo.
    * Issue this command: ```mill Risk.ui.fastOpt```
    * In case you want the optimized version: ```mill Risk.ui.fullOpt```

2) Building the backend: ```mill Risk.backend.compile```
    * Before you build the backend make sure that the jsLocation in the [CustomLauncher](Risk/backend/src/launchers/CustomLauncher.scala) object
      points to the actual directory where the generated js file can be found.
      Given that, you can launch the app from your IDE by executing the CustomLauncher  object.

# Opening the project in IntelliJ:

Run this command in the root folder, before you try open the project in IntelliJ.

```
mill mill.scalalib.GenIdea/idea
```