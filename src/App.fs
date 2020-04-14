module App.View

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

type ValidationErrors =
    {
      Email : string list
      Password : string list
    } with
    member this.HasErrors =
        not (this.Email = List.empty && this.Password = List.empty)

    static member empty = { Email = []; Password = []}

type Model =
    {
      Email : string
      Password : string
      Errors : ValidationErrors
      ModalState: bool
}

type Msg =
    | ChangeEmail of string
    | ChangePassword of string
    | ValidateEmail
    | ValidatePassword
    | Submit
    | Clear
    | ToggleModal

let init _ = { Email = ""; Password = "" ; Errors = ValidationErrors.empty; ModalState = false}, Cmd.none

let validateEmail email =
      [ System.String.IsNullOrWhiteSpace(email), "Field Email cannot be empty"
        email.Trim().Length < 5, "An Email must at least have 5 characters"
        Seq.contains ('@') email |> not, "An Email should contain a '@' symbol"]
      |> List.filter fst
      |> List.map snd


let validatePassword password =
      [ System.String.IsNullOrWhiteSpace(password), "A Password cannot be empty"
        password.Trim().Length < 5, "Field Password must at least have 5 characters"
        Seq.exists (System.Char.IsUpper) password |> not, "A Password should contain a captial"]
      |> List.filter fst
      |> List.map snd

let private update msg model =
    match msg with
    | ChangePassword newPassword ->
        { (model:Model) with Password = newPassword }, Cmd.ofMsg ValidatePassword
    | ChangeEmail newEmail ->
        { model with Email = newEmail }, Cmd.ofMsg ValidateEmail
    | ValidatePassword  ->
        let passwordErrors = validatePassword model.Password
        let errors = { model.Errors with Password = passwordErrors }
        { model with Errors = errors  }, Cmd.none
    | ValidateEmail  ->
        let emailErrors = validateEmail model.Email
        let errors = { model.Errors with Email = emailErrors }
        { model with Errors = errors  }, Cmd.none
    | Submit ->
        { model with ModalState = not model.ModalState }, Cmd.none
    | Clear ->
        init()
    | ToggleModal ->
        { model with ModalState = not model.ModalState }, Cmd.none

let navBar =
    Navbar.navbar []
            [  Navbar.End.div [ ]

                    [ Control.div [ ]
                        [ Button.a [ Button.Props [ Href "https://github.com/CraigChamberlain/fable-validation-example-no-deps" ] ]
                            [ Icon.icon [ ] [ Fa.i [ Fa.Brand.Github ] [] ]
                              span [] [ str "Github" ]
                            ]
                         ]

                      Control.div [ ]
                        [ a [ Href "https://www.gitpod.io/#https://github.com/CraigChamberlain/fable-validation-example-no-deps"  ]
                              [Image.image [ Image.Props [ Alt "Gitpod" ]]
                                   [ img [ Src "https://gitpod.io/button/open-in-gitpod.svg" ]  ]

                      ]]]]

let private emailInput model dispatch =
  let errors = model.Errors.Email
  Field.div [ ]
            [ Label.label [ ]
                [ str "Email" ]
              Control.div [ Control.HasIconLeft
                            Control.HasIconRight ]
                [ Input.email [ Input.OnChange (fun ev -> dispatch (ChangeEmail ev.Value))
                                Input.Value model.Email
                                match errors with
                                | [] -> Input.Color IsSuccess
                                | _ -> Input.Color IsDanger
                               ]
                  Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                    [ Fa.i [ Fa.Solid.Envelope ]
                        [ ] ]
                  Icon.icon [ Icon.Size IsSmall; Icon.IsRight ]
                    [ Fa.i [ match errors with
                               | [] -> Fa.Solid.Check
                               | _ -> Fa.Solid.ExclamationTriangle ]
                        [ ] ] ]
              match errors with
              | [] ->
                  Help.help  [ Help.Color IsSuccess ] [ str "This Email is valid" ]
              | errorList ->
                  let errorListHtml = ul [] (errorList |> List.map (fun e -> li [ ] [ str e ]) )
                  Help.help  [ Help.Color IsDanger ] [errorListHtml]
            ]

let private passwordInput model dispatch =
       let errors = model.Errors.Password
       Field.div [ ]
            [ Label.label [ ]
                [ str "Password" ]
              Control.div [ Control.HasIconLeft
                            Control.HasIconRight ]
                [ Input.password
                          [
                             Input.OnChange (fun ev -> dispatch (ChangePassword ev.Value))
                             Input.Value model.Password
                             Input.Props [ AutoFocus true ]
                             match errors with
                             | [] -> Input.Color IsSuccess
                             | _ -> Input.Color IsDanger
                          ]
                  Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                    [ Fa.i [ Fa.Solid.User ]
                        [ ] ]
                  Icon.icon [ Icon.Size IsSmall; Icon.IsRight ]
                    [ Fa.i [
                              match errors with
                               | [] -> Fa.Solid.Check
                               | _ -> Fa.Solid.ExclamationTriangle
                         ]
                        [ ] ] ]


              match errors with
              | [] ->
                  Help.help  [ Help.Color IsSuccess ] [ str "This Password is valid" ]
              | errorList ->
                  let errorListHtml = ul [] (errorList |> List.map (fun e -> li [ ] [ str e ]) )
                  Help.help  [ Help.Color IsDanger ] [errorListHtml]
            ]


// Render the modal
let basicModal  model dispatch =
    Modal.modal [ Modal.IsActive model.ModalState ]
        [ Modal.background [ Props [ OnClick (fun _ -> dispatch ToggleModal) ] ] [ ]
          Modal.content [ ]
            [ Box.box' [ ]
                [
                  Heading.h2 [] [str "Email:"]
                  str model.Email
                  Heading.h2 [] [str "Password:"]
                  str model.Password  ] ]
          Modal.close [ Modal.Close.Size IsLarge
                        Modal.Close.OnClick (fun _ -> dispatch ToggleModal) ] [ ] ]

let private view model dispatch =
  div [] [
    navBar
    Hero.hero [ Hero.IsFullHeight ]
        [ Hero.body [ ]
            [ Container.container [ ]
                [ Columns.columns [ Columns.CustomClass "has-text-centered" ]
                    [ Column.column [ Column.Width(Screen.All, Column.IsOneThird)
                                      Column.Offset(Screen.All, Column.IsOneThird) ]
                        [ Image.image [ Image.Is128x128
                                        Image.Props [ Style [ Margin "auto"] ] ]
                            [ img [ Src "assets/fulma_logo.svg" ] ]
                          form [ ]
                              [ emailInput model dispatch
                                passwordInput model dispatch
                                // Control area (submit, cancel, etc.)
                                Field.div [ Field.IsGrouped ]
                                  [ Control.div [ ]
                                      [ Button.button
                                          [
                                            Button.Color IsPrimary
                                            Button.Props
                                              [ Type "button"
                                                OnClick (fun _ -> dispatch Submit)
                                                Disabled <| model.Errors.HasErrors
                                              ]
                                            ]
                                          [ str "Submit" ]
                                       ]
                                    Control.div [ ]
                                      [ Button.button [
                                          Button.IsLink
                                          Button.Props
                                              [ Type "button"
                                                OnClick (fun _ -> dispatch Clear)
                                              ]
                                          ]
                                          [ str "Clear" ] ] ]


                          ]
                          div [ ]
                             [ basicModal model dispatch ]

        ] ] ] ] ] ]

open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
