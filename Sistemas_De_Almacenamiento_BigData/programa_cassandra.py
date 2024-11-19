# Importación de librerias necesarias para conexión con Cassandra y gestión de fechas
import datetime

from cassandra.cluster import Cluster
from datetime import date


#--------- Parte 3: Programa Python de gestión de datos --------#

# 1º: Definición de clases de las entidades y relaciones

# Clases para las entidades
class Paciente:
    def __init__(self, DNI, Nombre, Fecha_Nac, Direccion, Tlf, Alergias):
        self.DNI = DNI
        self.Nombre = Nombre
        self.Fecha_Nac = Fecha_Nac
        self.Direccion = Direccion
        self.Tlf = Tlf
        self.Alergias = Alergias

class Cita:
    def __init__(self, IdCita, Fecha_Hora, Motivo):  # Constructor con relación 1:n entre Cliente y Pedido
        self.IdCita = IdCita
        self.Fecha_Hora = Fecha_Hora
        self.Motivo = Motivo

class Tratamiento:
    def __init__(self, IdTratamiento, Descripcion, Costo):
        self.IdTratamiento = IdTratamiento
        self.Descripcion = Descripcion
        self.Costo = Costo

class Consultorio:
    def __init__(self, Numero, Hospital, Ciudad):
        self.Numero = Numero
        self.Hospital = Hospital
        self.Ciudad = Ciudad
        
class Medico:
    def __init__(self, DNI, Nombre, Fecha_Nac, Tlf, Especialidades):
        self.DNI = DNI
        self.Nombre = Nombre
        self.Fecha_Nac = Fecha_Nac
        self.Tlf = Tlf
        self.Especialidades = Especialidades
        
class Receta:
    def __init__(self, IdReceta, Fecha_Emision):
        self.IdReceta = IdReceta
        self.Fecha_Emision = Fecha_Emision


class Medicamento:
    def __init__(self, Codigo, Nombre, Dosis):
        self.Codigo = Codigo
        self.Nombre = Nombre
        self.Dosis = Dosis

class Numcitas:
    def __init__(self, DNI, Nombre, Numcitas):
        self.DNI = DNI
        self.Nombre = Nombre
        self.Numcitas = Numcitas

# Clases para Relaciones
    # Relaciones 1:n que podamos utilizar
class R_PacienteCita:
    def __init__(self, DNI, Nombre, IdCita):
        self.DNI = DNI
        self.Nombre = Nombre
        self.IdCita = IdCita

    #Relaciones n:m
class R_RecetaMedicamento:
    def __init__(self, Fecha_Emision, IdReceta, CodigoMedicamento, NombreMedicamento, DosisMedicamento):
        self.Fecha_Emision = Fecha_Emision
        self.IdReceta = IdReceta
        self.CodigoMedicamento = CodigoMedicamento
        self.NombreMedicamento = NombreMedicamento
        self.DosisMedicamento = DosisMedicamento



# 2º Creación de métodos de inserción de datos


# Función para pedir datos de un paciente e insertarlos en las tablas 1, 6 y soporte
def insertPaciente():
    # Pedimos al usuario del programa los datos del paciente
    Nombre = input("Dame nombre del paciente")

    DNI = input("Dame el DNI del paciente (número entero): ").strip()
    while not DNI.isnumeric():
        DNI = input("El DNI debe ser un número entero. Dame el DNI del paciente: ").strip()
    DNI = int(DNI)

    Fecha_Nac = input("Dame la fecha de nacimiento del paciente (yyyy-mm-dd)")
    Direccion = input("Dame direccion del paciente")
    Tlf = input("Dame el número de teléfono del paciente")

    Alergias = set()  # iniciamos la colección (set) que contendra las alergias a insertar
    Alergia = input("Introduzca una alergia, enter para parar")
    while (Alergia != ""):
        Alergias.add(Alergia)
        Alergia = input("Introduzca una alergia, enter para parar")

    p = Paciente(DNI, Nombre, Fecha_Nac, Direccion, Tlf, Alergias)

    # Insertar en Tabla1
    insertStatementTabla1 = session.prepare(
    "INSERT INTO Tabla1 (paciente_nombre, paciente_dni, paciente_fecha_nac, paciente_direccion, paciente_tlf, paciente_alergias) VALUES (?, ?, ?, ?, ?, ?)")
    session.execute(insertStatementTabla1, [p.Nombre, p.DNI, p.Fecha_Nac, p.Direccion, p.Tlf, p.Alergias])


    # Insertar en Tabla 6
    insertStatementTabla6 = session.prepare(
        "INSERT INTO Tabla6 (paciente_alergia, paciente_dni, paciente_nombre, paciente_alergias) VALUES (?, ?, ?, ?)")
	#insertar en alergias por paciente
    for Alerg in Alergias:
        session.execute(insertStatementTabla6, [Alerg, p.DNI, p.Nombre, p.Alergias])

    # Insertar en Tabla Soporte Paciente
# table soporte_paciente(paciente_dni int, paciente_nombre text, paciente_fecha_nac date, direecion text, tlf text, alergias set<text>, PRIMARY KEY(paciente_dni));
    insertStatementTablaSoportePaciente = session.prepare(
    "INSERT INTO soporte_paciente (paciente_dni, paciente_nombre, paciente_fecha_nac, paciente_direccion, paciente_tlf, paciente_alergias) VALUES (?, ?, ?, ?, ?, ?)")
    session.execute(insertStatementTablaSoportePaciente, [p.DNI, p.Nombre, p.Fecha_Nac, p.Direccion, p.Tlf, p.Alergias])
    print("Datos insertados")



# Función para pedir datos de un médico e insertar los datos en la Tabla soporte
def insertMedico():
    # Pedimos al usuario del programa los datos del médico
    Nombre = input("Dame nombre del médico")

    DNI = input("Dame el DNI del médico (número entero): ").strip()
    while not DNI.isnumeric():
        DNI = input("El DNI debe ser un número entero. Dame el DNI del médico: ").strip()
    DNI = int(DNI)

    Fecha_Nac = input("Dame la fecha de nacimiento del médico (yyyy-mm-dd)")
    Tlf = input("Dame el número de teléfono del médico")

    Especialidades = set()  # iniciamos la colección (set) que contendra las especialidades a insertar
    Especialidad = input("Introduzca una especialidad, enter para parar")
    while (Especialidad != ""):
        Especialidades.add(Especialidad)
        Especialidad = input("Introduzca una especialidad, enter para parar")

    m = Medico(DNI, Nombre, Fecha_Nac, Tlf, Especialidades)

    # Insertar en Tabla Soporte Médico
    insertStatementTablaSoporteMedico = session.prepare(
    "INSERT INTO soporte_medico (medico_dni, medico_nombre, medico_fecha_nac, medico_tlf, medico_especialidades) VALUES (?, ?, ?, ?, ?)")
    session.execute(insertStatementTablaSoporteMedico, [m.DNI, m.Nombre, m.Fecha_Nac, m.Tlf, m.Especialidades])
    print("Datos insertados")



# Función para insertar datos en la relación = TIENE
# Esta relación está hecha entre Paciente y Cita
# La tabla afectada será la Tabla4, que contiene el número de citas que tiene un paciente
# Como vamos a utilizar una tabla soporte para obtener la información del paciente, creamos la siguiente función:
def extraerDatosPaciente(DNI):
    select = session.prepare(
        "SELECT * FROM soporte_paciente WHERE paciente_dni = ?"
    )
    filas = session.execute(select, [DNI])  # No es necesario el ',' con una sola variable en la lista
    # Si no hay resultados, devolvemos `None` o lanzamos una excepción
    if not filas:
        return None  # O podrías usar: raise ValueError("Paciente no encontrado")
    # Si hay resultados, se ejecuta el bucle y retorna el paciente
    for fila in filas:
        p = Paciente(DNI, fila.paciente_nombre, fila.paciente_fecha_nac, fila.paciente_direccion, fila.paciente_tlf, fila.paciente_alergias)
        return p  # Devuelve el objeto `Paciente` con los datos encontrados


def insertRelacionPacienteCitas():
    # Pedimos al usuario del programa los datos del paciente. Vamos a pedir solo el DNI y obtenemos el nombre mediante la tabla soporte.
    DNI = input("Dame el dni del paciente")
    DNI = int(DNI)
    p = extraerDatosPaciente(DNI) 
    if p is None:  # Si no se encuentra a el paciente, mostramos un mensaje y detenemos la función
        print("Este paciente no existe. Introduce sus datos (Opción 1)")
        return
    
    nombre = p.Nombre  # Obtenemos a través de la tabla soporte el nombre de la persona

    idCita = input("Dame el número de citas") #lo suyo sería hacer un if el idCita está en la tabla de citas o no, para que no se cuenten duplicados, pero esta tabla no la tenemos

    insertStatementNumCitas = session.prepare(
    "UPDATE tabla4 SET numcitas = numcitas + 1 WHERE paciente_dni = ? AND paciente_nombre = ?") # Aumentamos el número de citas de esas personas
    session.execute(insertStatementNumCitas, [DNI, nombre])
    print("Datos insertados")



# Función para insertar datos en la relación = CONTIENE
# Esta relación está hecha entre Receta y Medicamento, y tiene una cardinalidad de n:m
# La tabla en la que vamos a insertar los datos es la Tabla5
# Tabla 5: Obtener todas las asociaciones entre recetas y medicamentos a través de la fecha de receta
# Ni para Receta ni para Medicamento tenemos tabla soporte, por lo que necesitaremos pedir toda la información necesaria
#Tabla 5 pide receta.fecha_emision, receta_id, medicamento_codigo, medicamento_nombre, medicamento dosis
def insertRelacionRecetaMedicamento():
    # Pedimos al usuario del programa los datos de la Receta y el Medicamento
    receta_id = input("Dame el ID de la Receta")
    while not receta_id.isnumeric():
        receta_id = input("El ID de la receta debe ser un número entero. Dame el ID de la receta:").strip()
    receta_id = int(receta_id)
    receta_fecha_emision= input("Dame la Fecha de Emisión de la receta (yyyy-mm-dd)") 
    medicamento_codigo = input("Dame el código del medicamento")
    while not medicamento_codigo.isnumeric():
        medicamento_codigo = input("El código del medicamento debe ser un número entero. Dame el código del medicamento:").strip()
    medicamento_codigo = int(medicamento_codigo)
    medicamento_nombre = input("Dame el nombre del medicamento")
    medicamento_dosis = input("Dame la dosis del medicamento")

    # Insertar en Tabla5
    insertStatementTabla5 = session.prepare(
    "INSERT INTO Tabla5 (receta_fecha_emision, receta_id, medicamento_codigo, medicamento_nombre, medicamento_dosis) VALUES (?, ?, ?, ?, ?)")
    session.execute(insertStatementTabla5, [receta_fecha_emision, receta_id, medicamento_codigo, medicamento_nombre, medicamento_dosis])
    print("Datos insertados")





# 3º Creación de funciones de actualización de datos
# Actualizar la dirección de un paciente según su DNI en la Tabla1. Utilizar la tabla soporte.
# Utilizaremos la función de extraerDatosPaciente() para llamar a la tabla soporte
def updateDireccionPaciente():
    while True:
        DNI = input("Dame el DNI del paciente:")
        while not DNI.isnumeric():
            DNI = input("El DNI de la receta debe ser un número entero. Dame el DNI de la receta:").strip()
        DNI = int(DNI)
        p = extraerDatosPaciente(DNI)
        # Si se encuentra el paciente, salimos del bucle
        if p is not None:
            break
        # Si no se encuentra la persona en la tabla de Cassandra, mostramos un mensaje y pedimos otro DNI
        print("Paciente no encontrado. Por favor, introduce un DNI válido.")
    # Ahora que hemos encontrado un paciente, podemos trabajar con sus datos
    DNI = int(DNI)
    nombre = p.Nombre
    direccion_antigua = p.Direccion
    nueva_direccion = input(f"La dirección actual de ' {nombre} ' es ' {direccion_antigua} '. Introduce una nueva si quieres, o pulsa enter para mantener la actual:")
    # Si el usuario pulsa espacio (deja en blanco), no se actualiza nada
    if nueva_direccion.strip() == "":
        print("La dirección no ha cambiado.")
        return  # Termina la función sin actualizar
    else:
        updateDireccionTabla1 = session.prepare(
        "UPDATE Tabla1 SET paciente_direccion = ? WHERE paciente_nombre = ? AND paciente_DNI = ?")
        session.execute(updateDireccionTabla1, [nueva_direccion, nombre, DNI])
        print(f"La dirección de {nombre} ha sido actualizada a {nueva_direccion}")




# 4º Creación de funciones de borrado de datos
# Borrar con la fecha de la receta, la relación entre receta y medicamento de la insertStatementTabla5
def deleteRelacionRecetaMedicamento():
    while True: #Hacemos esto para crear un bucle donde no se pare de pedir la fecha
        # Solicitamos la fecha de las recetas a eliminar
        receta_fecha_emision = input("Dame la fecha de las recetas que deseas eliminar (yyyy-mm-dd) o pulsa intro para salir:")
        
        # Si el ususario pulsa espacio, salimos
        if receta_fecha_emision.strip() == '': 
            print('Operación cerrada')
            return

        # Verificamos si la fecha existe en la tabla
        selectFecha = session.prepare(
            "SELECT * FROM Tabla5 WHERE receta_fecha_emision = ? LIMIT 1"
        )
        resultado = session.execute(selectFecha, [receta_fecha_emision])
        
        # Si no se encuentra ninguna fila con esa fecha, volvemos a entrar en el bucle y pedimos la fecha nuevamente
        if not resultado:
            print(f"No se encontró ninguna receta con la fecha {receta_fecha_emision}. Por favor, introduce una fecha válida.")
        else:
            # Si la fecha existe, procedemos con la eliminación
            deleteTabla5 = session.prepare(
                "DELETE FROM Tabla5 WHERE receta_fecha_emision = ?"
            )
            session.execute(deleteTabla5, [receta_fecha_emision])
            print(f"Las recetas con la fecha {receta_fecha_emision} han sido eliminadas.")
            break  # Salir del bucle una vez eliminadas las recetas


# 5º Creación de funciones de consulta de información general
# Para cada consulta, vamos a crear la función para extraer la información de la BBDD, y después, la función para pedir los valores

# Vamos a realizar consulta general para consultar la Tabla1. Solicitaremos el nombre de un paciente y mostraremos su info ------------------------------------
def extraerTabla1(nombre):
    select = session.prepare(
        "SELECT * FROM Tabla1 WHERE paciente_nombre = ?")  # solo va a devolver una filia pero lo tratamos como si fuesen varias
    filas = session.execute(select, [nombre, ])
    pacientes = []
    for fila in filas:
        p = Paciente( fila.paciente_dni, fila.paciente_nombre, fila.paciente_fecha_nac, fila.paciente_direccion, fila.paciente_tlf, fila.paciente_alergias)
        pacientes.append(p)
    return pacientes
    pass

def consultaDatosTabla1():
    nombre = input(
        "Introduzca el nombre del paciente a buscar") 
    pacientes = extraerTabla1(nombre)
    for paciente in pacientes:
        print("Nombre: ", paciente.Nombre)
        print("DNI: ", paciente.DNI)
        print("Fecha de nacimiento: ", paciente.Fecha_Nac)
        print("Dirección: ", paciente.Direccion)
        print("Teléfono: ", paciente.Tlf)
        print("Alergias: ", paciente.Alergias)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Vamos a realizar consulta general para consultar la Tabla2. Solicitaremos el DNI de un médico y mostraremos la info de las citas que tiene ---------------------
def extraerTabla4(Nombre, DNI):
    DNI = int(DNI)
    select = session.prepare(
        "SELECT * FROM Tabla4 WHERE paciente_nombre = ? AND paciente_dni = ?")  # solo va a devolver una filia pero lo tratamos como si fuesen varias
    filas = session.execute(select, [Nombre, DNI ,])
    pacientes = []
    for fila in filas:
        p = Numcitas( fila.paciente_dni, fila.paciente_nombre, fila.numcitas)
        pacientes.append(p)
    return pacientes
    pass

def consultaDatosTabla4():
    nombre = input(
        "Introduzca el nombre del paciente a buscar") 
    DNI = input(
    "Introduzca el DNI del paciente a buscar") 
    DNI = int(DNI)

    pacientes = extraerTabla4(nombre, DNI)

    for paciente in pacientes:
        print("Nombre: ", paciente.Nombre)
        print("DNI: ", paciente.DNI)
        print("Número de citas: ", paciente.Numcitas)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Vamos a realizar consulta general para consultar la Tabla4, la de la relación TIENE. Solicitaremos el DNI y nombre de un paciente y mostraremos la info del número de citas que tiene -----
def extraerTabla4(Nombre, DNI):
    DNI = int(DNI)
    select = session.prepare(
        "SELECT * FROM Tabla4 WHERE paciente_nombre = ? AND paciente_dni = ?")  # solo va a devolver una filia pero lo tratamos como si fuesen varias
    filas = session.execute(select, [Nombre, DNI ,])
    pacientes = []
    for fila in filas:
        p = Numcitas( fila.paciente_dni, fila.paciente_nombre, fila.numcitas)
        pacientes.append(p)
    return pacientes
    pass

def consultaDatosTabla4():
    nombre = input(
        "Introduzca el nombre del paciente a buscar") 
    DNI = input(
    "Introduzca el DNI del paciente a buscar") 
    DNI = int(DNI)

    pacientes = extraerTabla4(nombre, DNI)

    for paciente in pacientes:
        print("Nombre: ", paciente.Nombre)
        print("DNI: ", paciente.DNI)
        print("Número de citas: ", paciente.Numcitas)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Vamos a realizar consulta general para consultar la Tabla 5, la de la relación CONTIENE. Solicitaremos la fecha de emisión y devolveremos todas las columnas de la tabla con esta fecha-------
def extraerTabla5(Fecha):
    select = session.prepare(
        "SELECT * FROM Tabla5 WHERE receta_fecha_emision = ?")  # solo va a devolver una filia pero lo tratamos como si fuesen varias
    filas = session.execute(select, [Fecha, ])
    recetas = []
    for fila in filas:
        r = R_RecetaMedicamento(fila.receta_fecha_emision, fila.receta_id, fila.medicamento_codigo, fila.medicamento_nombre, fila.medicamento_dosis)
        recetas.append(r)
    return recetas
    pass

def consultaDatosTabla5():
    Fecha = input(
        "Introduzca la fecha de emisión de las recetas a buscar (yyyy-mm-dd)") 
    recetas = extraerTabla5(Fecha)
    for receta in recetas:
        print("Fecha de emisión de la receta: ", receta.Fecha_Emision)
        print("Id Receta: ", receta.IdReceta)
        print("Código de medicamento: ", receta.CodigoMedicamento)
        print("Nombre de medicamento: ", receta.NombreMedicamento)
        print("Dosis de medicamento: ", receta.DosisMedicamento)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------


# Vamos a realizar consulta general para consultar la Tabla Soporte de los pacientes. Solicitaremos el DNI de un paciente y mostraremos su info-------------------
def extraerSoportePaciente(DNI):
    DNI = int(DNI)
    select = session.prepare(
        "SELECT * FROM soporte_paciente WHERE paciente_dni = ?")  # solo va a devolver una filia pero lo tratamos como si fuesen varias
    filas = session.execute(select, [DNI, ])
    pacientes = []
    for fila in filas:
        p = Paciente( fila.paciente_dni, fila.paciente_nombre, fila.paciente_fecha_nac, fila.paciente_direccion,  fila.paciente_tlf, fila.paciente_alergias)
        pacientes.append(p)
    return pacientes
    pass

def consultaDatosSoportePaciente():
    DNI = input(
        "Introduzca el DNI del paciente a buscar") 
    DNI = int(DNI)
    pacientes = extraerSoportePaciente(DNI)
    for paciente in pacientes:
        print("Nombre: ", paciente.Nombre)
        print("DNI: ", paciente.DNI)
        print("Fecha de nacimiento: ", paciente.Fecha_Nac)
        print("Fecha de nacimiento: ", paciente.Direccion)
        print("Teléfono: ", paciente.Tlf)
        print("Especialidades: ", paciente.Alergias)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------


# Vamos a realizar consulta general para consultar la Tabla Soporte de los médicos. Solicitaremos el DNI de un médico y mostraremos su info-------------------
def extraerSoporteMedico(DNI):
    DNI = int(DNI)
    select = session.prepare(
        "SELECT * FROM soporte_medico WHERE medico_dni = ?")  # solo va a devolver una filia pero lo tratamos como si fuesen varias
    filas = session.execute(select, [DNI, ])
    medicos = []
    for fila in filas:
        m = Medico( fila.medico_dni, fila.medico_nombre, fila.medico_fecha_nac,  fila.medico_tlf, fila.medico_especialidades)
        medicos.append(m)
    return medicos
    pass

def consultaDatosSoporteMedico():
    DNI = input(
        "Introduzca el DNI del médico a buscar") 
    DNI = int(DNI)
    medicos = extraerSoporteMedico(DNI)
    for medico in medicos:
        print("Nombre: ", medico.Nombre)
        print("DNI: ", medico.DNI)
        print("Fecha de nacimiento: ", medico.Fecha_Nac)
        print("Teléfono: ", medico.Tlf)
        print("Especialidades: ", medico.Especialidades)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------





# Programa principal
# Conexión con Cassandra
cluster = Cluster()
# cluster = Cluster(['192.168.0.1', '192.168.0.2'], port=..., ssl_context=...)
session = cluster.connect('alejandroselles') #El nombre de mi keyspace
numero = -1
# Sigue pidiendo operaciones hasta que se introduzca 0
while (numero != 0):
    print("Introduzca un número para ejecutar una de las siguientes operaciones:")
    print("1. Insertar un paciente")
    print("2. Insertar un médico")
    print("3. Insertar relación TIENE entre pacientes y citas")
    print("4. Insertar relación CONTIENE entre recetas y medicamentos")
    print("5. Actualizar dirección de un paciente")
    print("6. Borrar la relacion receta-medicamento a partir de la fecha de emisión de la receta")
    print("7. Consultar datos de pacientes según el nombre (Tabla 1)")
    print("8. Consultar datos del número de citas de un paciente según su DNI (Relación TIENE - Tabla 4)")
    print("9. Consultar datos de asociaciones entre recetas y medicamentos a través de fecha de la receta (Relación CONTIENE - Tabla 5)")
    print("10. Consultar datos de la tabla soporte de paciente a partir de DNI")
    print("11. Consultar datos de la tabla soporte de médico a partir de DNI")
    print("0. Cerrar aplicación")

    numero = int(input())  # Pedimos numero al usuario
    if (numero == 1):
        insertPaciente()
    elif (numero == 2):
        insertMedico()
    elif (numero == 3):
        insertRelacionPacienteCitas()
    elif (numero == 4):
        insertRelacionRecetaMedicamento()
    elif (numero == 5):
        updateDireccionPaciente()
    elif (numero == 6):
        deleteRelacionRecetaMedicamento()
    elif (numero == 7):
        consultaDatosTabla1()
    elif (numero == 8):
        consultaDatosTabla4()
    elif (numero == 9):
        consultaDatosTabla5()
    elif (numero == 10):
        consultaDatosSoportePaciente()
    elif (numero == 11):
        consultaDatosSoporteMedico()
    else:
        print("Número incorrecto o 0")
cluster.shutdown()  # cerramos conexion