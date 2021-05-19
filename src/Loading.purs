module SambaDeUmaNotaSo.LoadingModal where

import Prelude

import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SambaDeUmaNotaSo.Action (Action)
import SambaDeUmaNotaSo.ModalUtil (modalBody, modalHeader)

loading :: forall w. { open :: Boolean } -> HH.HTML w Action
loading { open } =
  HH.div
    [ HP.classes $ map ClassName ([ "modal", "fixed", "w-full", "h-full", "top-0", "left-0", "flex", "items-center", "justify-center" ] <> if open then [] else [ "opacity-0", "pointer-events-none" ])
    ]
    [ HH.div
        [ HP.classes $ map ClassName [ "modal-overlay", "absolute", "w-full", "h-full", "bg-gray-900", "opacity-50" ]
        ]
        []
    , HH.div
        [ HP.classes $ map ClassName [ "modal-container", "bg-white", "mx-auto", "rounded", "shadow-lg", "z-50", "overflow-y-auto" ]
        ]
        [ HH.div
            [ HP.classes $ map ClassName [ "modal-content", "py-4", "text-left", "px-6" ]
            ]
            [ HH.div
                [ HP.classes $ map ClassName [ "flex", "justify-between", "items-center", "pb-3" ]
                ]
                [ modalHeader
                    [ HH.text "Samba de Uma Nota Só" ]
                ]
            , modalBody [ HH.text "Loading..." ]
            ]
        ]
    ]