# Risk

The classic strategy game Risk with the counties of Hungary. Implemented with the [gbge](https://github.com/kurgansoft/gbge) engine.

# How to play

* Make sure you have JRE >= 17 installed.
* Download the latest jar file from the releases section.
* The command ```java -jar risk.jar``` will launch the game server with default options:
    * Join the game with url: http://localhost:8080
    * Game board url: http://localhost:8080/s
* You can specify the host and port by providing VM options.
  For example to run with port 9999 and bind to IP address '192.168.0.1':
    * ``` java -jar -Dhost=192.168.0.1 -Dport=9999 risk.jar```