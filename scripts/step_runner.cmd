@echo off
set /p step="Which step number to run? "
cd ..\day%step%\build\libs
java -jar day%step%-all.jar