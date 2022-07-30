
// Project: AGK-Multiplayer Server 
// Created: 2022-07-27

#Company_Name "Dryagin"
#Option_Explicit

#Constant False 0
#Constant True  1

#Constant AGK_ErrorMode_Ignore 0
#Constant AGK_ErrorMode_Report 1
#Constant AGK_ErrorMode_Stop   2
#Constant AGK_SyncRate_Save     0
#Constant AGK_SyncRate_Accurate 1

#Constant TAB$   = Chr(0x09)
#Constant LF$    = Chr(0x0A)
#Constant CR$    = Chr(0x0D)
#Constant CRLF$  = Chr(0x0D) + Chr(0x0A)
#Constant CRLFCRLF$ = Chr(0x0D) + Chr(0x0A) + Chr(0x0D) + Chr(0x0A)
#Constant SPACE$ = Chr(0x20)
#Constant QUOT$  = Chr(0x22)
#Constant TIMES$ = Chr(0xD7)

// Состояния подключений
#Constant SocketFlag_Disconnected -3
#Constant SocketFlag_Unregistered -2
#Constant SocketFlag_Ready        -1
#Constant SocketFlag_Handshake     0
#Constant SocketFlag_Ping          1
#Constant SocketFlag_Spawn         2
#Constant SocketFlag_Despawn       3
#Constant SocketFlag_Position      4

// Переменные с параметрами сервера
IP As String = "127.0.0.1"      // IP-адрес сервера
port As Integer = 49152         // Порт для ПК и мобильных клиентов
webPort As Integer = 49153      // Порт для браузерных клиентов
Global pingDelay As Float = 3.0 // Задержка между запросами пинга
// Идентификаторы слушателей сокетов
socketListenerIndex As Integer
webSocketListenerIndex As Integer

// Переменные обработчика событий от клиентов
newSocketIndex As Integer
clientIndex As Integer
socketBytesAvailable As Integer

// Счетчик уникальных идентификаторов
Global lastUniqIndex As Integer = 100000

// Переменные для управления параметрами отображения
pMouseX As Float
pMouseY As Float
mouseX As Float
mouseY As Float
mouseWheel As Float

// Переменные для перемещения спрайтов
speed As Float = 50.0
delta As Float
x As Float
y As Float
dist As Float
spriteX As Float
spriteY As Float
teleportDist As Float = 100.0 // Если расстояние до конечной точки превышает данное значение, то спрайт клиента переместится немедленно

// Переменные для обновления протокола
tokenIndex As Integer
numOfTokens As Integer
line As String
key As String
request As String
response As String
responseLength As Integer
responseOffset As Integer

// Переменные для чтения пакетов WebSocket
firstByte As Integer
secondByte As Integer
bufferOffset As Integer
frameLength As Integer
payloadLength As Integer
maskingKey As Integer
payloadOffset As Integer
frameOffset As Integer
nameLength As Integer

// Клиент
Type ClientType
	
	socketIndex As Integer // Идентификатор подключения
	socketFlag As Integer  // Состояние подключения
	web As Integer         // Тип подключения: 0 - обычный клиент (windows/linux/mac/android/ios), 1 - WebSocket (html5)
	uniqIndex As Integer   // Уникальный идентификатор
	name As String         // Имя клиента
	spriteIndex As Integer // Идентификатор спрайта
	
	// Расположение
	x As Float
	y As Float
	
	// Сердцебиение
	pingTimer As Float  // Время отправки следующего запроса
	pingFlag As Integer // Состояние ответа на запрос
	
	// Для HTML5 клиентов
	bufferMemblockIndex As Integer   // Буфер принятых данных
	bufferMemblockOffset As Integer  // Смещение текущего байта в буфере принятых данных
	opcode As Integer                // Код операции принимаемых пакетов
	payloadMemblockIndex As Integer  // Полезные данные (собранные воедино и со снятой маской)
	payloadMemblockOffset As Integer // Размер полезных данных
	
EndType

Global Dim client [] As ClientType

// Добавить нового клиента
//
// socketIndex - идентификатор подключения
// web - использование WebSocket (для HTML5 клиентов)
// Возвращает идентификатор нового клиента

Function AddClient(socketIndex As Integer, web As Integer)
	client.length = client.length + 1
	client[client.length].socketIndex = socketIndex
	client[client.length].web = web
	client[client.length].socketFlag = SocketFlag_Disconnected
	client[client.length].pingTimer = Timer() + pingDelay
	client[client.length].pingFlag = True
	If web
		client[client.length].bufferMemblockIndex = CreateMemblock(65536)
		client[client.length].payloadMemblockIndex = CreateMemblock(65536)
	EndIf
EndFunction client.length

// Удалить клиента
//
// clientIndex - идентификатор клиента

Function DeleteClient(clientIndex As Integer)
	uniqIndex As Integer
	uniqIndex = client[clientIndex].uniqIndex
	If GetSpriteExists(client[clientIndex].spriteIndex) Then DeleteSprite(client[clientIndex].spriteIndex)
	If GetSocketExists(client[clientIndex].socketIndex) Then DeleteSocket(client[clientIndex].socketIndex)
	client.Remove(clientIndex)
	If uniqIndex
		For clientIndex = 0 To client.length
			SendDespawn(clientIndex, uniqIndex)
		Next clientIndex
	EndIf
EndFunction

// Отправить клиенту рукопожатие
//
// clientIndex - идентификатор клиента

Function SendHandshake(clientIndex As Integer)
	If client[clientIndex].web
		SendSocketByte(client[clientIndex].socketIndex, %10000010)
		SendSocketByte(client[clientIndex].socketIndex, 5)
	EndIf
	SendSocketByte(client[clientIndex].socketIndex, SocketFlag_Handshake)
	SendSocketInteger(client[clientIndex].socketIndex, client[clientIndex].uniqIndex)
	FlushSocket(client[clientIndex].socketIndex)
EndFunction

// Отправить клиенту пинг
//
// clientIndex - идентификатор клиента

Function SendPing(clientIndex As Integer)
	If client[clientIndex].web
		SendSocketByte(client[clientIndex].socketIndex, %10000010)
		SendSocketByte(client[clientIndex].socketIndex, 1)
	EndIf
	SendSocketByte(client[clientIndex].socketIndex, SocketFlag_Ping)
	FlushSocket(client[clientIndex].socketIndex)
EndFunction

// Отправить клиенту появление нового клиента
//
// clientIndex - идентификатор клиента-получателя
// spawnClientIndex - идентификатор клиента, который должен у него появиться

Function SendSpawn(clientIndex As Integer, spawnClientIndex As Integer)
	If client[clientIndex].web
		SendSocketByte(client[clientIndex].socketIndex, %10000010)
		SendSocketByte(client[clientIndex].socketIndex, 9 + ByteLen(client[spawnClientIndex].name))
	EndIf
	SendSocketByte(client[clientIndex].socketIndex, SocketFlag_Spawn)
	SendSocketInteger(client[clientIndex].socketIndex, client[spawnClientIndex].uniqIndex)
	SendSocketString(client[clientIndex].socketIndex, client[spawnClientIndex].name)
	FlushSocket(client[clientIndex].socketIndex)
EndFunction

// Отправить удаление клиента
//
// clientIndex - идентификатор клиента-получателя
// uniqIndex - уникальный идентификатор удаляемого клиента

Function SendDespawn(clientIndex As Integer, uniqIndex As Integer)
	If client[clientIndex].web
		SendSocketByte(client[clientIndex].socketIndex, %10000010)
		SendSocketByte(client[clientIndex].socketIndex, 5)
	EndIf
	SendSocketByte(client[clientIndex].socketIndex, SocketFlag_Despawn)
	SendSocketInteger(client[clientIndex].socketIndex, uniqIndex)
	FlushSocket(client[clientIndex].socketIndex)
EndFunction

// Отправить перемещение клиента
//
// clientIndex - идентификатор клиента-получателя
// positionClientIndex - идентификатор перемещаемого клиента

Function SendPosition(clientIndex As Integer, positionClientIndex As Integer)
	If client[clientIndex].web
		SendSocketByte(client[clientIndex].socketIndex, %10000010)
		SendSocketByte(client[clientIndex].socketIndex, 13)
	EndIf
	SendSocketByte(client[clientIndex].socketIndex, SocketFlag_Position)
	SendSocketInteger(client[clientIndex].socketIndex, client[positionClientIndex].uniqIndex)
	SendSocketFloat(client[clientIndex].socketIndex, client[positionClientIndex].x)
	SendSocketFloat(client[clientIndex].socketIndex, client[positionClientIndex].y)
	FlushSocket(client[clientIndex].socketIndex)
EndFunction

// Добавить (показать) представившегося клиента
//
// clientIndex - идентификатор клиент
// name - имя клиента

Function Spawn(clientIndex As Integer, name As String)
	subClientIndex As Integer
	client[clientIndex].name = name
	client[clientIndex].socketFlag = SocketFlag_Ready
	Inc lastUniqIndex
	client[clientIndex].uniqIndex = lastUniqIndex
	client[clientIndex].x = Random(0, 1920) - 960.0
	client[clientIndex].y = Random(0, 1080) - 540.0
	client[clientIndex].spriteIndex = CreateSprite(0)
	SetSpriteSize(client[clientIndex].spriteIndex, 32.0, 32.0)
	SetSpriteOffset(client[clientIndex].spriteIndex, 16.0, 16.0)
	SetSpritePositionByOffset(client[clientIndex].spriteIndex, client[clientIndex].x, client[clientIndex].y)
	SendHandshake(clientIndex)
	SendPosition(clientIndex, clientIndex)
	For subClientIndex = 0 To client.length
		If clientIndex <> subClientIndex
			// Отправляем новому клиенту спавн и расположения других клиентов
			SendSpawn(clientIndex, subClientIndex)
			SendPosition(clientIndex, subClientIndex)
			// Отправляем другим клиентам спавн и расположение нового клиента
			SendSpawn(subClientIndex, clientIndex)
			SendPosition(subClientIndex, clientIndex)
		EndIf
	Next subClientIndex
EndFunction

// Переместить клиента
//
// clientIndex - идентификатор клиент
// x, y - новое расположение

Function SetPosition(clientIndex As Integer, x As Float, y As Float)
	subClientIndex As Integer
	client[clientIndex].x = x
	client[clientIndex].y = y
	For subClientIndex = 0 To client.length
		If clientIndex <> subClientIndex
			SendPosition(subClientIndex, clientIndex)
		EndIf
	Next subClientIndex
EndFunction

SetErrorMode(AGK_ErrorMode_Stop)
SetWindowTitle("Server")
SetWindowSize(1280, 720, False)
SetWindowAllowResize(True)
SetVirtualResolution(1920, 1080)
SetOrientationAllowed(True, True, True, True)
SetSyncRate(60, AGK_SyncRate_Accurate)
SetScissor(0, 0, 0, 0)
UseNewDefaultFonts(True)
SetPrintSize(32.0)
SetPrintColor(0xFF, 0xFF, 0xFF)
SetClearColor(0x80, 0x80, 0x80)

SetViewZoom(1.0)
SetViewOffset(-960.0, -540.0)

socketListenerIndex = CreateSocketListener(IP, port)
webSocketListenerIndex = CreateSocketListener(IP, webPort)

Do
	
	// Принимаем подключения от обычных клиентов
	Do
		newSocketIndex = GetSocketListenerConnection(socketListenerIndex)
		If newSocketIndex
			AddClient(newSocketIndex, False)
		Else
			Exit
		EndIf
	Loop
	// Принимаем подключения от HTML5 клиентов
	Do
		newSocketIndex = GetSocketListenerConnection(webSocketListenerIndex)
		If newSocketIndex
			AddClient(newSocketIndex, True)
		Else
			Exit
		EndIf
	Loop
	
	// Принимаем данные
	For clientIndex = 0 To client.length
		If GetSocketExists(client[clientIndex].socketIndex)
			Select GetSocketConnected(client[clientIndex].socketIndex)
				Case -1 // Ошибка или разрыв соединения
					DeleteClient(clientIndex)
					Dec clientIndex
					Continue
				EndCase
				Case 1
					Do
						socketBytesAvailable = GetSocketBytesAvailable(client[clientIndex].socketIndex)
						
						If client[clientIndex].web
							
							// Обработка данных от HTML5 клиентов
							// Все принятые данные сразу записываются в блок данных клиента
							If socketBytesAvailable > 0
								While socketBytesAvailable > 0
									SetMemblockByteSigned(client[clientIndex].bufferMemblockIndex, client[clientIndex].bufferMemblockOffset, GetSocketByte(client[clientIndex].socketIndex))
									Inc client[clientIndex].bufferMemblockOffset
									Dec socketBytesAvailable
								EndWhile
							EndIf
							
							If client[clientIndex].socketFlag = SocketFlag_Disconnected
								// Из википедии [https://ru.wikipedia.org/wiki/WebSocket]:
								// Для установления соединения WebSocket клиент и сервер используют протокол, похожий на HTTP.
								// Клиент формирует особый HTTP-запрос, на который сервер отвечает определенным образом.
								request = GetMemblockString(client[clientIndex].bufferMemblockIndex, 0, client[clientIndex].bufferMemblockOffset)
								If FindStringCount(request, CRLFCRLF$)
									key = ""
									numOfTokens = CountStringTokens(request, CRLF$)
									// Находим в запросе строку, содержащую ключ, который нужен для ответа
									For tokenIndex = 1 To numOfTokens
										line = GetStringToken(request, CRLF$, tokenIndex)
										If Left(line, 17) = "Sec-WebSocket-Key"
											key = Mid(line, 20, -1)
										EndIf
									Next tokenIndex
									If key <> ""
										// Преобразуем ключ для ответа
										key = HexToBase64(Sha1(key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
										// Формируем ответ
										response = "HTTP/1.1 101 Switching Protocols" + CRLF$
										response = response + "Upgrade: websocket" + CRLF$
										response = response + "Connection: Upgrade" + CRLF$
										response = response + "Sec-WebSocket-Accept: " + key + CRLF$
										response = response + "Sec-WebSocket-Protocol: binary" + CRLFCRLF$
										// Помещаем ответ в блок памяти и отправляем его клиенту
										SetMemblockString(client[clientIndex].bufferMemblockIndex, 0, response)
										responseLength = ByteLen(response) - 1
										For responseOffset = 0 To responseLength
											SendSocketByte(client[clientIndex].socketIndex, GetMemblockByte(client[clientIndex].bufferMemblockIndex, responseOffset))
										Next responseOffset
										FlushSocket(client[clientIndex].socketIndex)
										client[clientIndex].bufferMemblockOffset = 0
										client[clientIndex].socketFlag = SocketFlag_Unregistered
									Else // Клиент прислал запрос не соответствующего содержания, либо без ключа
										DeleteClient(clientIndex)
										Dec clientIndex
										Exit
									EndIf
								Else // Запрос принят не до конца
									Exit
								EndIf
							Else // Работа с подключенным клиентом
								// Экспортированные в HTML5 проекты присылают только два кода операций,
								// поэтому здесь будет реализована упрощенная система обработки WebSocket пакетов,
								// достаточная для работы с Web-приложениями разработанными в AppGameKit
								If client[clientIndex].bufferMemblockOffset > 2
									// Сперва определяем размер тела (полезной нагрузки) пакета
									secondByte = GetMemblockByte(client[clientIndex].bufferMemblockIndex, 1)
									payloadLength = secondByte && %01111111
									If payloadLength = %01111110
										If client[clientIndex].bufferMemblockOffset > 4
											payloadLength = GetMemblockShort(client[clientIndex].bufferMemblockIndex, 2)
											bufferOffset = 4
										Else
											Exit
										EndIf
									ElseIf payloadLength = %01111111
										If client[clientIndex].bufferMemblockOffset > 10
											payloadLength = GetMemblockInt(client[clientIndex].bufferMemblockIndex, 2)
											bufferOffset = 10
										Else
											Exit
										EndIf
									Else
										bufferOffset = 2
									EndIf
									// Если данные замаскированы, то запоминаем ключ
									If secondByte && %10000000
										If client[clientIndex].bufferMemblockOffset > bufferOffset + 4
											maskingKey = GetMemblockInt(client[clientIndex].bufferMemblockIndex, bufferOffset)
											bufferOffset = bufferOffset + 4
										Else
											Exit
										EndIf
									Else
										maskingKey = 0
									EndIf
									// Если все тело было принято, то перемещаем данные в отдельный блок памяти
									frameLength = bufferOffset + payloadLength
									If client[clientIndex].bufferMemblockOffset => frameLength
										firstByte = GetMemblockByte(client[clientIndex].bufferMemblockIndex, 0)
										// В данном случае не играет роли, какой пакет сейчас пришёл,
										// так что можно сразу добавить данные в блок памяти для последующей обработки
										For payloadOffset = 0 To payloadLength - 1 Step 4
											SetMemblockInt(client[clientIndex].payloadMemblockIndex, client[clientIndex].payloadMemblockOffset + payloadOffset, GetMemblockInt(client[clientIndex].bufferMemblockIndex, bufferOffset + payloadOffset) ~~ maskingKey)
										Next payloadOffset
										client[clientIndex].payloadMemblockOffset = client[clientIndex].payloadMemblockOffset + payloadLength
										// При необходимости, смещаем отсавшиеся принятые данные в начало блока памяти
										If client[clientIndex].bufferMemblockOffset > frameLength
											For frameOffset = frameLength To client[clientIndex].bufferMemblockOffset
												SetMemblockInt(client[clientIndex].bufferMemblockIndex, frameOffset - frameLength, GetMemblockInt(client[clientIndex].bufferMemblockIndex, frameOffset))
											Next frameOffset
											client[clientIndex].bufferMemblockOffset = client[clientIndex].bufferMemblockOffset - frameLength
										Else
											client[clientIndex].bufferMemblockOffset = 0
										EndIf
										// Запоминаем код операции, если он ещё не присвоен
										If client[clientIndex].opcode = 0x0 Then client[clientIndex].opcode = firstByte && %00001111
										// Проверяем, был ли данный пакет последним
										If firstByte && %10000000
											If client[clientIndex].opcode = 0x2 // Обработка двоичных данных
												If client[clientIndex].payloadMemblockOffset => 1
													If client[clientIndex].socketFlag = SocketFlag_Unregistered
														// Принимаем рукопожатие от незарегистрированного клиента
														If GetMemblockByte(client[clientIndex].payloadMemblockIndex, 0) = SocketFlag_Handshake
															If client[clientIndex].payloadMemblockOffset => 5
																nameLength = GetMemblockInt(client[clientIndex].payloadMemblockIndex, 1)
																If client[clientIndex].payloadMemblockOffset => 5 + nameLength
																	Spawn(clientIndex, GetMemblockString(client[clientIndex].payloadMemblockIndex, 5, nameLength))
																	client[clientIndex].socketFlag = SocketFlag_Ready
																EndIf
															Else // Клиент прислал пакет неправильного размера
																DeleteClient(clientIndex)
																Dec clientIndex
																Exit
															EndIf
														Else // Клиент прислал не рукопожатие
															DeleteClient(clientIndex)
															Dec clientIndex
															Exit
														EndIf
													Else // Клиент уже зарегистрирован, прнимаем от него обычные пакеты
														Select GetMemblockByte(client[clientIndex].payloadMemblockIndex, 0)
															Case SocketFlag_Ping
																client[clientIndex].pingFlag = True
															EndCase
															Case SocketFlag_Position
																If client[clientIndex].payloadMemblockOffset => 9
																	SetPosition(clientIndex, GetMemblockFloat(client[clientIndex].payloadMemblockIndex, 1), GetMemblockFloat(client[clientIndex].payloadMemblockIndex, 5))
																Else
																	Exit
																EndIf
															EndCase
															Case Default // Неизвестный тип пакета
																DeleteClient(clientIndex)
																Dec clientIndex
																Exit
															EndCase
														EndSelect
													EndIf
												EndIf
											Else
												// Закрытие соединения браузером (client[clientIndex].opcode = 0x8) или неизвестный код операции
												DeleteClient(clientIndex)
												Dec clientIndex
												Exit
											EndIf
											client[clientIndex].payloadMemblockOffset = 0
											client[clientIndex].opcode = 0x0
										Else // Принятый пакет не является завершающим
											Exit
										EndIf
									Else // Принято меньше данных, чем указано в заголовке пакета
										Exit
									EndIf
								Else // Клиент прислал пакет меньше двух байт
									// Этого недостаточно чтобы проверить предполагаемый конечный размер всего пакета
									Exit
								EndIf
							EndIf
							
						Else // Обработка данных от обычных клиентов
							
							Select client[clientIndex].socketFlag
								Case SocketFlag_Disconnected, SocketFlag_Unregistered
									If socketBytesAvailable => 5
										If GetSocketByte(client[clientIndex].socketIndex) = SocketFlag_Handshake
											// GetSocketString - уязвимое место, т.к. если клиент отправит размер строки больше,
											// чем последующие данные, то сервер зависнет на этом месте, в ожидании оставшихся данных
											Spawn(clientIndex, GetSocketString(client[clientIndex].socketIndex))
										Else // Клиент прислал первым пакетом не рукопожатие
											DeleteClient(clientIndex)
											Dec clientIndex
											Exit
										EndIf
									Else
										Exit
									EndIf
								EndCase
								Case SocketFlag_Ready
									If socketBytesAvailable => 1
										client[clientIndex].socketFlag = GetSocketByte(client[clientIndex].socketIndex)
									Else
										Exit
									EndIf
								EndCase
								Case SocketFlag_Ping
									client[clientIndex].pingFlag = True
									client[clientIndex].socketFlag = SocketFlag_Ready
								EndCase
								Case SocketFlag_Position
									If socketBytesAvailable => 8
										SetPosition(clientIndex, GetSocketFloat(client[clientIndex].socketIndex), GetSocketFloat(client[clientIndex].socketIndex))
										client[clientIndex].socketFlag = SocketFlag_Ready
									Else
										Exit
									EndIf
								EndCase
							EndSelect
							
						EndIf
						
					Loop
				EndCase
			EndSelect
		Else // Соединение отсутствует
			DeleteClient(clientIndex)
			Dec clientIndex
			Continue
		EndIf
	Next clientIndex
	
	// Сердцебиение. Отправляем пакет пинга клиентам, и, если он не ответил на предыдущий, то удаляем неответившего клиента.
	// Для упрощения сделал так, что ответ ожидается в течении интервала отправки пинга.
	// Лучшим решением будет ввести отдельный таймер, который будет срабатывать в момент Timer() + pingAwaiting
	// где pingAwaiting - время пределельного ожидания ответа на пинг, т.е. максимально-допустимый пинг для клиента
	For clientIndex = 0 To client.length
		If client[clientIndex].pingTimer <= Timer()
			If client[clientIndex].pingFlag
				SendPing(clientIndex)
				client[clientIndex].pingTimer = Timer() + pingDelay
				client[clientIndex].pingFlag = False
			Else // Клиент не ответил на пинг
				DeleteClient(clientIndex)
				Dec clientIndex
				Continue
			EndIf
		EndIf
	Next clientIndex
	
	// Перемещение спрайта от его текущего (устаревшего) расположения, до настоящего
	// Такой подход позволяет сделать перемещение более плавным, в ущерб актуальности расположения клиента,
	// и компромиссной точности (будут временные расхождения при смене направления)
	For clientIndex = 0 To client.length
		If client[clientIndex].spriteIndex
			spriteX = GetSpriteXByOffset(client[clientIndex].spriteIndex)
			spriteY = GetSpriteYByOffset(client[clientIndex].spriteIndex)
			If client[clientIndex].x <> spriteX Or client[clientIndex].y <> spriteY
				delta = speed * GetFrameTime()
				x = client[clientIndex].x - spriteX
				y = client[clientIndex].y - spriteY
				dist = Sqrt(x * x + y * y)
				If dist > teleportDist
					spriteX = client[clientIndex].x
					spriteY = client[clientIndex].y
				ElseIf dist > delta
					spriteX = spriteX + delta * x / dist
					spriteY = spriteY + delta * y / dist
				Else
					spriteX = client[clientIndex].x
					spriteY = client[clientIndex].y
				EndIf
				SetSpritePositionByOffset(client[clientIndex].spriteIndex, spriteX, spriteY)
			EndIf
		EndIf
	Next clientIndex
	
	Print("Количество клиентов: " + Str(client.length + 1) + CRLF$)
	
	// Вспомогательная часть для управления отображением (смещением вида и масштабом)
	// Не несет никакую полезную нагрузку для этого примера
	pMouseX = mouseX
	pMouseY = mouseY
	mouseX = GetRawMouseX()
	mouseY = GetRawMouseY()
	mouseWheel = GetRawMouseWheelDelta()
	If GetRawMouseRightState()
		SetViewOffset(GetViewOffsetX() - (mouseX - pMouseX) / GetViewZoom(), GetViewOffsetY() - (mouseY - pMouseY) / GetViewZoom())
	EndIf
	If mouseWheel <> 0.0
		mouseX = ScreenToWorldX(mouseX)
		mouseY = ScreenToWorldY(mouseY)
		If mouseWheel < 0.0
			SetViewZoom(GetViewZoom() * 0.5)
		ElseIf mouseWheel > 0.0
			SetViewZoom(GetViewZoom() * 2.0)
		EndIf
		SetViewOffset(mouseX - GetRawMouseX() / GetViewZoom(), mouseY - GetRawMouseY() / GetViewZoom())
		mouseX = GetRawMouseX()
		mouseY = GetRawMouseY()
	EndIf
	DrawLine(WorldToScreenX(0.0), WorldToScreenY(0.0), WorldToScreenX(0.0) + 100.0, WorldToScreenY(0.0), 0xFF0000FF, 0xFF0000FF)
	DrawLine(WorldToScreenX(0.0), WorldToScreenY(0.0), WorldToScreenX(0.0), WorldToScreenY(0.0) + 100.0, 0xFF00FF00, 0xFF00FF00)
	Print("FPS: " + Str(ScreenFPS(), 1) + CRLF$)
	Print("View")
	Print(TAB$ + "offset: " + Str(GetViewOffsetX(), 2) + ", " + Str(GetViewOffsetY(), 2))
	Print(TAB$ + "zoom: " + TIMES$ + Str(GetViewZoom(), 4) + CRLF$)
	Print("Mouse")
	Print(TAB$ + "screen: " + Str(mouseX, 2) + ", " + Str(mouseY, 2))
	Print(TAB$ + "world: " + Str(ScreenToWorldX(mouseX), 2) + ", " + Str(ScreenToWorldY(mouseY), 2))
	Sync()
	
Loop
