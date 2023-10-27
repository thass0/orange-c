{-# language QuasiQuotes #-}

module OrangeC
    ( compile
    ) where

import Data.ByteString
import Data.ByteString.Char8 as B8
import Text.RawString.QQ

compile :: ByteString -> ByteString
compile _ = B8.pack [r|
	.text
	.global main
	.type  main, @function
	.p2align 4

main:
	movl $2, %eax
	ret

	.section .note.GNU-stack,"",@progbits
|]
