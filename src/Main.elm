module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Element as El
import Element.Background as Bg
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


xLength : Int
xLength =
    4


yLength : Int
yLength =
    4


zLength : Int
zLength =
    4


numCells : Int
numCells =
    xLength * yLength * zLength


tripleFromCellNum : Int -> ( Int, Int, Int )
tripleFromCellNum cn =
    ( cn
    , cn
    , cn
    )


main : Program Value Float Float
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view
        , subscriptions = \_ -> onAnimationFrameDelta Basics.identity
        , update = \dt theta -> ( theta + dt / 5000, Cmd.none )
        }


view : Float -> Html Float
view theta =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Bg.gradient
            { angle = theta * 10 - theta * 4
            , steps = [ El.rgb 0.3 0 0.4, El.rgb 0 0.4 0.6 ]
            }
        ]
    <|
        El.column
            [ El.width El.fill, El.height El.fill, El.padding 10 ]
            [ El.el
                [ Font.color <| El.rgb 1 1 1
                , Font.size 50
                ]
              <|
                El.text "Hello Space"
            , El.el
                [ El.width El.fill, El.height El.fill ]
              <|
                El.el
                    [ El.centerX
                    , El.centerY
                    ]
                <|
                    El.html <|
                        WebGL.toHtml
                            [ width 500
                            , height 500
                            , style "display" "block"
                            ]
                            [ WebGL.entity
                                vertexShader
                                fragmentShader
                                cubeMesh
                                (uniforms theta)
                            ]
            ]


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Float -> Uniforms
uniforms theta =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (10 * theta) (vec3 0 1 0))
            (Mat4.makeRotate (4 * theta) (vec3 1 0 0))
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.9
    }



-- Mesh


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


cubeMesh : Mesh Vertex
cubeMesh =
    let
        sp =
            2

        l =
            0.5

        rtf =
            vec3 l l l

        ltf =
            vec3 -l l l

        lbf =
            vec3 -l -l l

        rbf =
            vec3 l -l l

        rbb =
            vec3 l -l -l

        rtb =
            vec3 l l -l

        ltb =
            vec3 -l l -l

        lbb =
            vec3 -l -l -l

        cube : Vec3 -> Vec3 -> List (List ( Vertex, Vertex, Vertex ))
        cube color offset =
            [ face color
                -- red right
                (Vec3.add rtf offset)
                (Vec3.add rbf offset)
                (Vec3.add rbb offset)
                (Vec3.add rtb offset)
            , face color
                -- aqua front
                (Vec3.add rtf offset)
                (Vec3.add rbf offset)
                (Vec3.add lbf offset)
                (Vec3.add ltf offset)
            , face color
                -- yellow top
                (Vec3.add rtf offset)
                (Vec3.add ltf offset)
                (Vec3.add ltb offset)
                (Vec3.add rtb offset)
            , face color
                -- green bottom
                (Vec3.add rbf offset)
                (Vec3.add lbf offset)
                (Vec3.add lbb offset)
                (Vec3.add rbb offset)
            , face color
                -- purple left
                (Vec3.add ltf offset)
                (Vec3.add lbf offset)
                (Vec3.add lbb offset)
                (Vec3.add ltb offset)
            , face color
                -- blue back
                (Vec3.add rtb offset)
                (Vec3.add rbb offset)
                (Vec3.add lbb offset)
                (Vec3.add ltb offset)
            ]

        cubePos color ( x, y, z ) =
            cube color
                (vec3 ((2 * x - 3) * sp / 2)
                    ((2 * y - 3) * sp / 2)
                    ((2 * z - 3) * sp / 2)
                )

        black =
            vec3 0 0 0

        gray =
            vec3 0.6 0.6 0.6

        yellow =
            vec3 1 0.85 0
    in
    (cubePos black ( 0, 0, 0 )
        ++ cubePos black ( 0, 0, 1 )
        ++ cubePos black ( 0, 0, 2 )
        ++ cubePos black ( 0, 0, 3 )
        ++ cubePos gray ( 0, 1, 0 )
        ++ cubePos gray ( 0, 1, 1 )
        ++ cubePos gray ( 0, 1, 2 )
        ++ cubePos gray ( 0, 1, 3 )
        ++ cubePos gray ( 0, 2, 0 )
        ++ cubePos gray ( 0, 2, 1 )
        ++ cubePos gray ( 0, 2, 2 )
        ++ cubePos gray ( 0, 2, 3 )
        ++ cubePos gray ( 0, 3, 0 )
        -- yellow
        ++ cubePos gray ( 0, 3, 1 )
        ++ cubePos gray ( 0, 3, 2 )
        ++ cubePos gray ( 0, 3, 3 )
        ++ cubePos black ( 1, 0, 0 )
        ++ cubePos black ( 1, 0, 1 )
        ++ cubePos black ( 1, 0, 2 )
        ++ cubePos black ( 1, 0, 3 )
        ++ cubePos gray ( 1, 1, 0 )
        ++ cubePos gray ( 1, 1, 1 )
        ++ cubePos gray ( 1, 1, 2 )
        ++ cubePos gray ( 1, 1, 3 )
        ++ cubePos gray ( 1, 2, 0 )
        ++ cubePos gray ( 1, 2, 1 )
        ++ cubePos gray ( 1, 2, 2 )
        ++ cubePos gray ( 1, 2, 3 )
        ++ cubePos gray ( 1, 3, 0 )
        ++ cubePos gray ( 1, 3, 1 )
        ++ cubePos gray ( 1, 3, 2 )
        ++ cubePos gray ( 1, 3, 3 )
        ++ cubePos black ( 2, 0, 0 )
        ++ cubePos black ( 2, 0, 1 )
        ++ cubePos black ( 2, 0, 2 )
        ++ cubePos black ( 2, 0, 3 )
        ++ cubePos gray ( 2, 1, 0 )
        ++ cubePos gray ( 2, 1, 1 )
        ++ cubePos gray ( 2, 1, 2 )
        ++ cubePos gray ( 2, 1, 3 )
        ++ cubePos gray ( 2, 2, 0 )
        ++ cubePos gray ( 2, 2, 1 )
        ++ cubePos gray ( 2, 2, 2 )
        ++ cubePos gray ( 2, 2, 3 )
        ++ cubePos gray ( 2, 3, 0 )
        ++ cubePos gray ( 2, 3, 1 )
        ++ cubePos gray ( 2, 3, 2 )
        ++ cubePos gray ( 2, 3, 3 )
        ++ cubePos black ( 3, 0, 0 )
        ++ cubePos black ( 3, 0, 1 )
        ++ cubePos black ( 3, 0, 2 )
        ++ cubePos black ( 3, 0, 3 )
        ++ cubePos gray ( 3, 1, 0 )
        ++ cubePos gray ( 3, 1, 1 )
        ++ cubePos gray ( 3, 1, 2 )
        ++ cubePos gray ( 3, 1, 3 )
        ++ cubePos gray ( 3, 2, 0 )
        ++ cubePos gray ( 3, 2, 1 )
        ++ cubePos gray ( 3, 2, 2 )
        ++ cubePos gray ( 3, 2, 3 )
        ++ cubePos gray ( 3, 3, 0 )
        ++ cubePos gray ( 3, 3, 1 )
        ++ cubePos gray ( 3, 3, 2 )
        ++ cubePos gray ( 3, 3, 3 )
    )
        |> List.concat
        |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
    let
        vertex position =
            Vertex color position
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 5.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 0.25);
        }

    |]
