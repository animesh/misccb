let (cg,at) = Data.List.partition (\x->x=='c'||x=='g') . concat . filter ((/='>').head) . lines $ ">header\naaacccggttt\n>header\naaacccggttt" in fromIntegral (length cg) / fromIntegral (length cg+length at)