@echo off
set /p step="Run solutions for day: "
cd ..\day%step%\build\libs
java -jar day%step%-all.jar