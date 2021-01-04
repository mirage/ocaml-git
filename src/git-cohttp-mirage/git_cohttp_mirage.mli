include Smart_git.HTTP

val conduit : Cohttp_mirage.Client.ctx Mimic.value
val with_conduit : Cohttp_mirage.Client.ctx -> Mimic.ctx -> Mimic.ctx
