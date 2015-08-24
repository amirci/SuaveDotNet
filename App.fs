open System.IO
open System
open Suave
open Suave.Web
open Suave.Http
open Suave.Types
open Suave.Http.Successful
open Suave.Http.Redirection
open Suave.Http.Files
open Suave.Http.RequestErrors
open Suave.Http.Applicatives
open Suave.Razor
open Suave.Types.Codes
open Suave.Http
open Suave.Http.Files
open Suave.Utils

open RazorEngine
open RazorEngine.Configuration
open RazorEngine.Templating
open RazorEngine.Text

type Bar = { foo: string }

let curDir = Directory.GetCurrentDirectory()
let fullPath = (+) curDir >> Path.GetFullPath
let fromHome = (+) "/../../"
let viewsFolder  = "Views"  |> fromHome |> fullPath
let assetsFolder = "Assets" |> fromHome |> fullPath

type TemplateManager () =
    let layoutRoots = [viewsFolder]

    interface ITemplateManager with

        member this.AddDynamic(key, source) = 
            raise (NotSupportedException "Not supported dynamic")

        member this.Resolve key =
            let full = key :?> FullPathTemplateKey
            if full = null then raise (new NotSupportedException("You can only use FullPathTemplateKey with this manager"))
            let template = File.ReadAllText(full.FullPath)
            new LoadedTemplateSource(template, full.FullPath) :> ITemplateSource

        member this.GetKey(name, resolveType, context) =
            let existing () = new FullPathTemplateKey(name, name, resolveType, context)
            let findFile () =
                layoutRoots
                |> List.tryPick (fun l ->
                    let p = Path.Combine(l, name)
                    [p; (p + ".cshtml")] |> List.tryFind (File.Exists)
                ) 
                |> function
                | Some file -> FullPathTemplateKey(name, file, resolveType, context) 
                | _ -> raise (name |> sprintf "File not found %s" |> InvalidOperationException)
            
            (if File.Exists(name) then existing() else findFile()) :> ITemplateKey


let viewBag = new DynamicViewBag()
let sc = TemplateServiceConfiguration()
sc.DisableTempFileLocking <- true
sc.CachingProvider <- new DefaultCachingProvider(fun t -> ())
sc.TemplateManager <- new TemplateManager()

// sc.BaseTemplateType <- typeof<HtmlSupportTemplateBase<_>>
let rs1 = RazorEngineService.Create(sc)
    
let razorView<'a> (path:string) (model : 'a) =
    fun r ->
      async {
            let content = rs1.RunCompile(path, typeof<Bar>, model, viewBag)
            return! Response.response HTTP_200 (UTF8.bytes content) r
        }


let wpgDotNet = 

    choose [
        GET >>= choose [
            path "/" >>= razorView "index.cshtml" {foo="Something"}
            pathRegex "(.*)\.(css|png|eot|svg|ttf|woff|otf)" >>= Files.browse assetsFolder
        ]        
    ]

//[<EntryPoint>]
//let main argv = 
//    startWebServer defaultConfig wpgDotNet
//    0 // return an integer exit code

startWebServer defaultConfig wpgDotNet