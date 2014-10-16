market <- function(rutabagas)
{
    money <- 0
    return(list(
        numberRutabagas = function()
        {
            return(rutabagas)
        },
        revenue = function()
        {
            return(money)
        },
        harvestRutabagas = function(amount)
        {
            rutabagas <<- rutabagas + amount
        },
        sellRutabagas = function(amount)
        {
            if(rutabagas >= amount)
                {
                    rutabagas <<- rutabagas - amount
                    money <<- money + amount*0.5
                }
            else
                {
                    warning("We do not have that many rutabagas")
                }
            return(rutabagas)
        }))
}

farmerJoe <- market(20)
farmerJoe$numberRutabagas()
farmerJoe$sellRutabagas(6)
farmerJoe$numberRutabagas()
farmerJoe$sellRutabagas(15)
farmerJoe$harvestRutabagas(10)
farmerJoe$numberRutabagas()
