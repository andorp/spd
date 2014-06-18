
{-
Sample problem from Chapter 2.2
-}

baseTicketPrice = 5
baseAttendance = 120
dollarChangeRate = 0.1
peopleChangeRate = 15
costFixedPart = 180
variableCosts = 0.04

attendees ticketPrice = baseAttendance - ((ticketPrice - baseTicketPrice) * (peopleChangeRate / dollarChangeRate))

revenue ticketPrice = ticketPrice * (attendees ticketPrice)

cost ticketPrice = costFixedPart + (variableCosts * (attendees ticketPrice))

profit ticketPrice = revenue ticketPrice - cost ticketPrice

