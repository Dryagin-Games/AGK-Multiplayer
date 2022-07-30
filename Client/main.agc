
// Project: AGK-Multiplayer Client 
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
#Constant SPACE$ = Chr(0x20)
#Constant QUOT$  = Chr(0x22)
#Constant TIMES$ = Chr(0xD7)

// Состояния подключения
#Constant SocketFlag_Disconnected -2
#Constant SocketFlag_Connected    -1
#Constant SocketFlag_Handshake     0
#Constant SocketFlag_Ping          1
#Constant SocketFlag_Spawn         2
#Constant SocketFlag_Despawn       3
#Constant SocketFlag_Position      4

// Клиент
Type ClientType
	uniqIndex As Integer
	name As String
	x As Float
	y As Float
	spriteIndex As Integer
EndType
Global Dim client [0] As ClientType
client[0].name = "Tester"

// Переменные с параметрами подключения
IP As String = "127.0.0.1"
port As Integer
If GetDeviceBaseName() = "html5"
	port = 49153
Else
	port = 49152
EndIf
socketIndex As Integer
socketFlag As Integer = SocketFlag_Disconnected
clientIndex As Integer

// Размеры ожидаемых пакетов (рукопожатие - 4 байта, пинг - 0 байт, спавн - 8 байт, деспавн - 4 байта, расположение - 12 байт)
Dim socketBytesAwaiting [] As Integer = [4, 0, 8, 4, 12]

// Отправка данных о расположении клиента
sendPositionTimer As Float       // Таймер отправки расположения
sendPositionDelay As Float = 0.1 // Задержка между отправками расположений

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

// Определить идентификатор клиента по его уникальному идентификатору
//
// uniqIndex - уникальный идентификатор
// Возвращает идентификатор клиента

Function GetClientIndexByUniqIndex(uniqIndex As Integer)
	clientIndex As Integer
	For clientIndex = 0 To client.length
		If client[clientIndex].uniqIndex = uniqIndex
			ExitFunction clientIndex
		EndIf
	Next clientIndex
EndFunction -1

// Создать спрайт клиента
//
// clientIndex - идентификатор клиента

Function CreateClientSprite(clientIndex As Integer)
	client[clientIndex].spriteIndex = CreateSprite(0)
	SetSpriteSize(client[clientIndex].spriteIndex, 32.0, 32.0)
	SetSpriteOffset(client[clientIndex].spriteIndex, 16.0, 16.0)
EndFunction

SetErrorMode(AGK_ErrorMode_Stop)
SetWindowTitle("Client")
SetWindowSize(1280, 720, False)
SetWindowAllowResize(True)
SetVirtualResolution(1920, 1080)
SetOrientationAllowed(True, True, True, True)
SetSyncRate(60, AGK_SyncRate_Accurate)
SetScissor(0, 0, 0, 0)
UseNewDefaultFonts(True)
SetPrintSize(32.0)
SetPrintColor(0xFF, 0xFF, 0xFF)
SetClearColor(0x40, 0x80, 0xC0)

SetViewZoom(1.0)
SetViewOffset(-960.0, -540.0)

Do
	
	PrintC("Состояние: ")
	If GetSocketExists(socketIndex)
		Select GetSocketConnected(socketIndex)
			Case -1 // Сокет отключен или не может подключиться
				Print("неудача" + CRLF$)
				DeleteSocket(socketIndex)
			EndCase
			Case 0 // Находится в процессе подключения
				// Сюда можно добавить код какого-нибудь индикатора ожидания подключения, например троббера
				Print("в процессе..." + CRLF$)
			EndCase
			Case 1 // Сокет подключен
				Print("подключен" + CRLF$)
				If socketFlag = SocketFlag_Disconnected
					// Здесь можно добавить код, который будет выполнен при установке соединения с сервером
					// Например - отправка рукопожатия, данные авторизации и т.д.
					// В этом примере будет отправлено только имя
					SendSocketByte(socketIndex, SocketFlag_Handshake)
					SendSocketString(socketIndex, client[0].name)
					FlushSocket(socketIndex)
					socketFlag = SocketFlag_Connected
				Else
					
					// Обработка принимаемых данных
					Do
						If socketFlag = SocketFlag_Connected
							If GetSocketBytesAvailable(socketIndex) => 1
								socketFlag = GetSocketByte(socketIndex)
								If socketFlag = SocketFlag_Ping
									SendSocketByte(socketIndex, SocketFlag_Ping)
									FlushSocket(socketIndex)
									socketFlag = SocketFlag_Connected
								EndIf
							Else
								Exit
							EndIf
						Else
							If GetSocketBytesAvailable(socketIndex) => socketBytesAwaiting[socketFlag]
								Select socketFlag
									Case SocketFlag_Handshake
										client[0].uniqIndex = GetSocketInteger(socketIndex)
										CreateClientSprite(0)
										sendPositionTimer = Timer() + sendPositionDelay
									EndCase
									Case SocketFlag_Spawn
										client.length = client.length + 1
										client[client.length].uniqIndex = GetSocketInteger(socketIndex)
										client[client.length].name = GetSocketString(socketIndex)
										CreateClientSprite(client.length)
									EndCase
									Case SocketFlag_Despawn
										clientIndex = GetClientIndexByUniqIndex(GetSocketInteger(socketIndex))
										DeleteSprite(client[clientIndex].spriteIndex)
										client.Remove(clientIndex)
									EndCase
									Case SocketFlag_Position
										clientIndex = GetClientIndexByUniqIndex(GetSocketInteger(socketIndex))
										x = GetSocketFloat(socketIndex)
										y = GetSocketFloat(socketIndex)
										If clientIndex <> -1
											client[clientIndex].x = x
											client[clientIndex].y = y
										EndIf
									EndCase
								EndSelect
								socketFlag = SocketFlag_Connected
							Else
								Exit
							EndIf
						EndIf
					Loop
					
					// Отправка координат клиента
					If Timer() => sendPositionTimer
						SendSocketByte(socketIndex, SocketFlag_Position)
						SendSocketFloat(socketIndex, client[0].x)
						SendSocketFloat(socketIndex, client[0].y)
						FlushSocket(socketIndex)
						sendPositionTimer = sendPositionTimer + sendPositionDelay
					EndIf
					
				EndIf
			EndCase
		EndSelect
	Else // Установка подключения
		Print("подключене" + CRLF$)
		If socketFlag <> SocketFlag_Disconnected
			While client.length > 0
				If GetSpriteExists(client[client.length].spriteIndex) Then DeleteSprite(client[client.length].spriteIndex)
				client.Remove(client.length)
			EndWhile
			If GetSpriteExists(client[0].spriteIndex) Then DeleteSprite(client[0].spriteIndex)
		EndIf
		socketIndex = ConnectSocket(IP, port, 3000)
		socketFlag = SocketFlag_Disconnected
	EndIf
	
	If socketFlag <> SocketFlag_Disconnected
		
		// Перемещение по нажатию на стрелки
		If GetRawKeyState(37)     // Влево
			client[0].x = client[0].x - speed * GetFrameTime()
		ElseIf GetRawKeyState(39) // Вправо
			client[0].x = client[0].x + speed * GetFrameTime()
		EndIf
		If GetRawKeyState(38)     // Вверх
			client[0].y = client[0].y - speed * GetFrameTime()
		ElseIf GetRawKeyState(40) // Вниз
			client[0].y = client[0].y + speed * GetFrameTime()
		EndIf
		
		If GetSpriteExists(client[0].spriteIndex)
			SetSpritePositionByOffset(client[0].spriteIndex, client[0].x, client[0].y)
		EndIf
		
		// Перемещение спрайта от его текущего (устаревшего) расположения, до настоящего
		// Такой подход позволяет сделать перемещение более плавным, в ущерб актуальности расположения клиента,
		// и компромиссной точности (будут временные расхождения при смене направления)
		For clientIndex = 1 To client.length
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
		
	EndIf
	
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
