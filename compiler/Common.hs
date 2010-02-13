
module Yadorigi.Common (amap,writeLater) where

import Control.Applicative

amap :: a -> [a -> b] -> [b]
amap = (<**>).pure

writeLater :: a
writeLater = error "Yadorigi.Common.writeLater"
