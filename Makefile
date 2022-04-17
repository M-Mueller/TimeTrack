restore:
	dotnet tool restore
	dotnet restore

build-server: restore
	dotnet build

build-client: restore
	cd Client && dotnet fable src --run webpack

build: build-server build-client

DEV_WEBROOT=$$PWD/Client/public

watch-server: restore
	export ASPNETCORE_ENVIRONMENT=Development; \
	export ASPNETCORE_WEBROOT=$(DEV_WEBROOT); \
	dotnet watch run --project Server

watch-client: restore
	cd Client && dotnet fable watch src --run webpack
