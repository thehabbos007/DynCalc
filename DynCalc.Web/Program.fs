module DynCalc.Web.App

open System

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe
open DynCalc.Web.ActorServices
open DynCalc.Web.HttpHandlers


// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    choose [ GET >=> route "/" >=> htmlFile "www/index.html"
             subRoute
                 "/api"
                 (choose [ GET
                           >=> choose [ route "/registry" >=> handleRegistry
                                        routef "/data/%i" (handleSinkData) ]
                           POST
                           >=> choose [ route "/program" >=> handleNewProgram
                                        route "/programAst" >=> handleProgramAst ]
                           DELETE
                           >=> choose [ routef "/undeploy/%i" (handleUndeploy) ] ])
             setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")

    clearResponse
    >=> setStatusCode 500
    >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder
        .WithOrigins("*")
        .AllowAnyMethod()
        .AllowAnyHeader()
    |> ignore

let configureApp (app: IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()

    app.UseStaticFiles() |> ignore

    (match env.IsDevelopment() with
     | true -> app.UseDeveloperExceptionPage()
     | false ->
         app
             .UseGiraffeErrorHandler(errorHandler)
             .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services
        .AddCors()
        .AddSingleton<IActorService, ActorService>()
        .AddSingleton<ISinkServiceMap, SinkServiceMap>()
        .AddGiraffe()
    |> ignore

let configureLogging (builder: ILoggingBuilder) =
    builder.AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main args =
    // let contentRoot = IO.Directory.GetCurrentDirectory()
    // let webRoot = IO.Path.Combine(contentRoot, "www")
    // printfn "%A" webRoot

    Host
        .CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun webHostBuilder ->
            webHostBuilder
                .Configure(Action<IApplicationBuilder> configureApp)
                .ConfigureServices(configureServices)
                .ConfigureLogging(configureLogging)
                .UseWebRoot("www")
            |> ignore)
        .Build()
        .Run()

    0
