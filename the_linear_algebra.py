from math import sqrt, ceil, floor, log, e, cbrt
from typing import Tuple, Union, Iterable, Any, Type, TypeAlias
from decimal import Decimal as de, getcontext, Decimal
from fractions import Fraction as fr, Fraction
from numbers import Number

MathExprStr: TypeAlias = str


class MathError(TypeError):
    def __new__(cls, message: str = "因数学运算导引发该错误"):
        return super().__new__(cls, message)


class MathZeroDivError(MathError):
    def __new__(cls, message: str = "因以0为除数引发该错误"):
        return super().__new__(cls, message)


class MathRootingError(MathError):
    def __new__(cls, message: str = "因对负数开偶次方根引发该错误"):
        return super().__new__(cls, message)


class MathPowError(MathError):
    def __new__(cls, message: str = "因计算0**0引发该错误"):
        return super().__new__(cls, message)


# 配置Decimal精度
getcontext().prec = 8


# 1D 标量类
class Scalar(de):
    """扩展Decimal实现更多数学运算的标量类"""

    def __new__(cls, value: Union[int, float, str, de] = 0) -> 'Scalar':
        cls.value = value
        cls._congruence_a = None
        cls._congruence_b = None
        return super().__new__(cls, value)

    def __pow__(self, power: Union[int, float, de, fr, 'Scalar']) -> 'Scalar':
        if self == 0 and power == 0:
            raise MathPowError("底数和指数不能都为0")
        if self == 0 and power < 0:
            raise MathPowError("底数为0指数就不能为负")
        if isinstance(power, (de, Scalar)):
            return Scalar(self.value ** power.value)
        return Scalar(self.value ** power)

    @staticmethod
    def sqrt(self) -> 'Scalar':
        """计算平方根"""
        if self < 0:
            raise MathRootingError()
        return Scalar(sqrt(float(self)))

    @staticmethod
    def cbrt(self) -> 'Scalar':
        """计算立方根"""
        return Scalar(cbrt(float(self)))

    @staticmethod
    def log(self, base: float = e) -> 'Scalar':
        """计算对数"""
        if float(self) <= 0:
            raise ValueError("对数的底数必须为正数")
        return Scalar(log(float(self), base))

    @staticmethod
    def rooting(other: 'Scalar', self: 'Scalar') -> 'Scalar':
        """计算self的other次方根"""
        if float(other) == 0:
            raise ValueError("根指数不能为0")
        return self ** (Scalar(1) / other)

    def congruence(self):
        """

        :return:
        """
        class _Scalar:
            def __init__(self, math_tool):
                self.math_tool = math_tool

            def __or__(self, other):
                if self.math_tool._congruence_a is None:
                    if not isinstance(other, Number):
                        raise TypeError(f"不支持的类型: {type(other)}")
                    self.math_tool._congruence_a = other
                    return self
                else:
                    if not isinstance(other, Number):
                        raise TypeError(f"不支持的类型: {type(other)}")
                    self.math_tool._congruence_b = other
                    return self

            def __invert__(self):
                return self

            def __mod__(self, m):
                if not isinstance(m, Number) or m == 0:
                    raise ValueError("模数必须是非零数字")
                if self.math_tool._congruence_a is None or self.math_tool._congruence_b is None:
                    raise ValueError("请使用 a |mt.congruence()| b % m 格式")

                result = (self.math_tool._congruence_a - self.math_tool._congruence_b) % m == 0

                self.math_tool._congruence_a = None
                self.math_tool._congruence_b = None

                return result

        return _Scalar(self)


def knuth_arrow(a: Scalar, k: int, b: Scalar) -> Scalar:
    """高德纳箭头表示法: a↑^k b"""
    if k < 1:
        raise ValueError("高德纳箭头层级k必须≥1")
    if k == 1:
        return a * b
    elif k == 2:
        return a ** b
    else:
        result = b
        # 注意：对于k≥3且a较大时会导致运算结果极大，可能溢出
        for _ in range(int(a)):
            result = knuth_arrow(a, k - 1, result)
    return result


def tetration(a: Scalar, n: int, b: Scalar) -> Scalar:
    """迭代幂运算: a↑↑n = a^(a^(...^a)) (n次迭代，初始值为b)"""
    if n < 0:
        raise ValueError("迭代次数必须是非负整数")
    result = b
    for _ in range(n):
        result = a ** result
    return result


class function:
    total = 0  # 类属性，用于计数所有函数实例

    def __new__(cls, expr: MathExprStr, n: Union[int, float, de, fr, Scalar] = 1) -> "function":
        # 创建实例
        instance = super().__new__(cls)
        return instance

    def __init__(self, expr: MathExprStr, n: Union[int, float, de, fr, Scalar] = 1) -> None:
        # 为每个实例分配唯一编号
        self.num = function.total
        function.total += 1  # 递增类计数器
        self.n = n
        self.expr = expr

    def run(self) -> Union[int, float, de, fr, Scalar]:
        """执行表达式并返回结果"""
        n = self.n
        try:
            return eval(self.expr)
        except Exception as ex:
            raise RuntimeError(f"执行表达式 '{self.expr}' 时出错: {ex}")

    def __add__(self, other: "function") -> "function":
        """两个函数相加"""
        if not isinstance(other, function):
            raise TypeError("只能与function类型相加")
        return function(f"({self.expr}) + ({other.expr})", 1)

    def __sub__(self, other: "function") -> "function":
        """两个函数相减"""
        if not isinstance(other, function):
            raise TypeError("只能与function类型相减")
        return function(f"({self.expr}) - ({other.expr})", 1)

    def __mul__(self, other: Union[int, float, de, fr, Scalar, "function"]) -> "function":
        """函数与标量或其他函数相乘"""
        if isinstance(other, function):
            # 与另一个函数相乘
            return function(f"({self.expr}) * ({other.expr})", 1)
        elif isinstance(other, (int, float, de, fr, Scalar)):
            # 与标量相乘
            return function(f"({self.expr}) * {other}", 1)
        else:
            raise TypeError(f"不支持与{type(other)}类型相乘")

    def __rmul__(self, other: Union[int, float, de, fr, Scalar]) -> "function":
        """标量在左侧的乘法"""
        return function(f"{other} * ({self.expr})", 1)

    def __truediv__(self, other: Union[int, float, de, fr, Scalar, "function"]) -> "function":
        """函数与标量或其他函数相除（真除法）"""
        if isinstance(other, function):
            return function(f"({self.expr}) / ({other.expr})", 1)
        elif isinstance(other, (int, float, de, fr, Scalar)):
            return function(f"({self.expr}) / {other}", 1)
        else:
            raise TypeError(f"不支持与{type(other)}类型相除")

    def __floordiv__(self, other: Union[int, float, de, fr, Scalar, "function"]) -> "function":
        """函数与标量或其他函数相除（整数除法）"""
        if isinstance(other, function):
            return function(f"({self.expr}) // ({other.expr})", 1)
        elif isinstance(other, (int, float, de, fr, Scalar)):
            return function(f"({self.expr}) // {other}", 1)
        else:
            raise TypeError(f"不支持与{type(other)}类型进行整数除法")


class Sequence:
    def __new__(cls, *args: Union[float, int, de, fr, Scalar]) -> "Sequence":
        instance = super().__new__(cls)
        instance.list = list(args)
        return instance

    def __getitem__(self, num: int) -> Union[int, float, de, fr, Scalar]:
        return self.list[num]

    def __len__(self) -> int:
        return len(self.list)

    def append(self, item: Union[int, float, de, fr, Scalar]) -> None:
        self.list.append(item)

    def insert(self, index: int, item: Union[int, float, de, fr, Scalar]) -> None:
        self.list.insert(index, item)

    def delete(self, index: int) -> None:
        self.list.remove(index)

    def clear(self) -> None:
        self.list.clear()

    def __repr__(self) -> str:
        t = list()
        for i in self.list:
            t.append(i)
        return f"Sequence{tuple(t)}"

    def __iter__(self) -> float | int | Decimal | Fraction | Scalar:
        for i in self.list:
            return i


def sequence(*args: Union[int, float, de, fr, Scalar]) -> Type[Sequence]:
    n = Sequence
    for i in args:
        n.append(i)
    return n


def fibonacci_sequence(index: int) -> Sequence:
    if index <= 0:
        raise TypeError("请填正整数")
    n = Sequence()
    a, b = 1, 1
    for _ in range(index - 1):
        n.append(a)
        a, b = b, a + b
    n.append(b)
    return n


# the_2D 向量类
class Vector2D:
    """二维向量类"""

    def __init__(self, x: Union[int, float, de, fr, Scalar], y: Union[int, float, de, fr, Scalar]) -> None:
        self.x = x
        self.y = y

    def __add__(self, other: 'Vector2D') -> 'Vector2D':
        if not isinstance(other, Vector2D):
            raise TypeError("只能与Vector2D类型相加")
        return Vector2D(self.x + other.x, self.y + other.y)

    def __sub__(self, other: 'Vector2D') -> 'Vector2D':
        if not isinstance(other, Vector2D):
            raise TypeError("只能与Vector2D类型相减")
        return Vector2D(self.x - other.x, self.y - other.y)

    def __mul__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        return Vector2D(self.x * scalar, self.y * scalar)

    def __rmul__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        return self.__mul__(scalar)

    def __truediv__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        if scalar == 0:
            raise MathZeroDivError("除数不能为0")
        return Vector2D(self.x / scalar, self.y / scalar)

    def __floordiv__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        if scalar == 0:
            raise MathZeroDivError("除数不能为0")
        return Vector2D(self.x // scalar, self.y // scalar)

    def __mod__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        if scalar == 0:
            raise MathZeroDivError("除数不能为0")
        return Vector2D(self.x % scalar, self.y % scalar)

    def __pos__(self) -> 'Vector2D':
        return Vector2D(+self.x, +self.y)

    def __neg__(self) -> 'Vector2D':
        return Vector2D(-self.x, -self.y)

    def __iadd__(self, other: 'Vector2D') -> 'Vector2D':
        return self + other

    def __isub__(self, other: 'Vector2D') -> 'Vector2D':
        return self - other

    def __imul__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        return self * scalar

    def __itruediv__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        return self / scalar

    def __ifloordiv__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        return self // scalar

    def __imod__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector2D':
        return self % scalar

    def __str__(self) -> str:
        return f"Vector2D({self.x}, {self.y})"

    def __repr__(self) -> str:
        return f"Vector2D(x={self.x!r}, y={self.y!r})"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, Vector2D):
            return False
        return self.x == other.x and self.y == other.y

    def __ne__(self, other: Any) -> bool:
        return not self.__eq__(other)

    def __lt__(self, other: Any) -> bool:
        if not isinstance(other, Vector2D):
            return False
        return self.x < other.x and self.y < other.y

    def __le__(self, other: Any) -> bool:
        return self.__eq__(other) or self.__lt__(other)

    def __gt__(self, other: Any) -> bool:
        return not self.__le__(other)

    def __ge__(self, other: Any) -> bool:
        return not self.__lt__(other)

    def __len__(self) -> int:
        return 2

    def __abs__(self) -> float:
        """计算向量模长"""
        return sqrt(float(self.x ** 2 + self.y ** 2))

    @property
    def mode(self) -> float:
        """向量模长属性"""
        return self.__abs__()

    def __bool__(self) -> bool:
        return self.x != 0 or self.y != 0

    def ceil(self) -> 'Vector2D':
        """对各分量向上取整"""
        return Vector2D(Scalar(ceil(self.x)), Scalar(ceil(self.y)))

    def floor(self) -> 'Vector2D':
        """对各分量向下取整"""
        return Vector2D(Scalar(floor(self.x)), Scalar(floor(self.y)))

    def normalize(self) -> 'Vector2D':
        """向量归一化"""
        mod = self.mode
        if mod == 0:
            raise ValueError("零向量无法归一化")
        return self / Scalar(mod)

    def dot(self, other: 'Vector2D') -> Union[int, float, de, fr, Scalar]:
        """向量点积"""
        if not isinstance(other, Vector2D):
            raise TypeError("点积运算需要两个Vector2D对象")
        return self.x * other.x + self.y * other.y

    def __iter__(self) -> Iterable[Union[int, float, de, fr, Scalar]]:
        yield self.x
        yield self.y


# the_2D 基向量
base_vector2D_i = Vector2D(1, 0)
base_vector2D_j = Vector2D(0, 1)


def vector2D(x: Union[int, float, de, fr, Scalar], y: Union[int, float, de, fr, Scalar]) -> Vector2D:
    """创建二维向量的工厂函数"""
    return Vector2D(x, y)


# the_2D 矩阵类
class Matrix2D:
    """二维矩阵类（2x2）"""

    def __init__(self, data: Iterable[Vector2D] = None) -> None:
        if data is None:
            # 使用基向量初始化单位矩阵
            data_list = [base_vector2D_i, base_vector2D_j]
        else:
            data_list = list(data)
            if len(data_list) != 2 or not all(isinstance(v, Vector2D) for v in data_list):
                raise TypeError('data必须是包含两个Vector2D实例的可迭代对象')
        self.rows = data_list  # 存储两行向量

    @property
    def i(self) -> Vector2D:
        return self.rows[0]

    @property
    def j(self) -> Vector2D:
        return self.rows[1]

    @property
    def det(self) -> int | float | Decimal | Fraction | complex:
        """计算矩阵行列式"""
        return self.i.x * self.j.y - self.i.y * self.j.x

    @property
    def tr(self) -> int | float | Decimal | Fraction | complex:
        return self.i.x + self.j.y

    def __mul__(self, other: Union['Matrix2D', Vector2D]) -> Union['Matrix2D', Vector2D]:
        if isinstance(self, Vector2D):
            return Vector2D(
                self.i.x * other.x + self.j.x * other.y,
                self.i.y * other.x + self.j.y * other.y
            )
        # 矩阵乘法：行×列
        row1 = Vector2D(
            self.i.x * other.i.x + self.i.y * other.j.x,
            self.i.x * other.i.y + self.i.y * other.j.y
        )
        row2 = Vector2D(
            self.j.x * other.i.x + self.j.y * other.j.x,
            self.j.x * other.i.y + self.j.y * other.j.y
        )
        return Matrix2D([row1, row2])

    def __str__(self) -> str:
        return f"Matrix2D({self.i}, {self.j})"

    def __repr__(self) -> str:
        return f"Matrix2D(rows={self.rows!r})"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, Matrix2D):
            return False
        return self.rows == other.rows

    def __ne__(self, other: Any) -> bool:
        return not self.__eq__(other)

    def transpose(self) -> 'Matrix2D':
        """矩阵转置"""
        return Matrix2D([
            Vector2D(self.i.x, self.j.x),
            Vector2D(self.i.y, self.j.y)
        ])

    def __iter__(self):
        yield from self.rows


# the_2D 单位矩阵
base_Matrix2D = Matrix2D()


def create_matrix2D(rows: Union[Tuple[Vector2D, Vector2D], Tuple[Union[int, float, de, fr, Scalar],
Union[int, float, de, fr, Scalar],
Union[int, float, de, fr, Scalar],
Union[int, float, de, fr, Scalar]]]) -> Matrix2D:
    """创建二维矩阵的工厂函数"""
    if isinstance(rows[0], Vector2D) and isinstance(rows[1], Vector2D):
        return Matrix2D(rows)
    elif len(rows) == 4 and all(isinstance(x, (int, float, de, fr, Scalar)) for x in rows):
        return Matrix2D([
            Vector2D(rows[0], rows[1]),
            Vector2D(rows[2], rows[3])
        ])
    else:
        raise TypeError("输入必须是两个Vector2D或四个Scalar的元组")


# the_3D 向量类
class Vector3D:
    """三维向量类"""

    def __init__(self, x: Union[int, float, de, fr, Scalar],
                 y: Union[int, float, de, fr, Scalar],
                 z: Union[int, float, de, fr, Scalar]) -> None:
        self.x = x
        self.y = y
        self.z = z

    def __add__(self, other: 'Vector3D') -> 'Vector3D':
        if not isinstance(other, Vector3D):
            raise TypeError("只能与Vector3D类型相加")
        return Vector3D(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: 'Vector3D') -> 'Vector3D':
        if not isinstance(other, Vector3D):
            raise TypeError("只能与Vector3D类型相减")
        return Vector3D(self.x - other.x, self.y - other.y, self.z - other.z)

    def __mul__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector3D':
        return Vector3D(self.x * scalar, self.y * scalar, self.z * scalar)

    def __rmul__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector3D':
        return self.__mul__(scalar)

    def __truediv__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector3D':
        if scalar == 0:
            raise MathZeroDivError("除数不能为0")
        return Vector3D(self.x / scalar, self.y / scalar, self.z / scalar)

    def __floordiv__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector3D':
        if scalar == 0:
            raise MathZeroDivError("除数不能为0")
        return Vector3D(self.x // scalar, self.y // scalar, self.z // scalar)

    def __mod__(self, scalar: Union[int, float, de, fr, Scalar]) -> 'Vector3D':
        if scalar == 0:
            raise MathZeroDivError("除数不能为0")
        return Vector3D(self.x % scalar, self.y % scalar, self.z % scalar)

    def __str__(self) -> str:
        return f"Vector3D({self.x}, {self.y}, {self.z})"

    def __repr__(self) -> str:
        return f"Vector3D(x={self.x!r}, y={self.y!r}, z={self.z!r})"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, Vector3D):
            return False
        return self.x == other.x and self.y == other.y and self.z == other.z

    def __ne__(self, other: Any) -> bool:
        return not self.__eq__(other)

    def __len__(self) -> int:
        return 3

    def __abs__(self) -> float:
        """计算向量模长"""
        return sqrt(float(self.x ** 2 + self.y ** 2 + self.z ** 2))

    @property
    def mode(self) -> float:
        """向量模长属性"""
        return self.__abs__()

    def __bool__(self) -> bool:
        return self.x != 0 or self.y != 0 or self.z != 0

    def ceil(self) -> 'Vector3D':
        """对各分量向上取整"""
        return Vector3D(
            Scalar(ceil(self.x)),
            Scalar(ceil(self.y)),
            Scalar(ceil(self.z))
        )

    def floor(self) -> 'Vector3D':
        """对各分量向下取整"""
        return Vector3D(
            Scalar(floor(self.x)),
            Scalar(floor(self.y)),
            Scalar(floor(self.z))
        )

    def normalize(self) -> 'Vector3D':
        """向量归一化"""
        mod = self.mode
        if mod == 0:
            raise ValueError("零向量无法归一化")
        return self / Scalar(mod)

    def dot(self, other: 'Vector3D') -> int | float | Decimal | Fraction | complex:
        """向量点积"""
        if not isinstance(other, Vector3D):
            raise TypeError("点积运算需要两个Vector3D对象")
        return self.x * other.x + self.y * other.y + self.z * other.z

    def cross(self, other: 'Vector3D') -> 'Vector3D':
        """向量叉积"""
        if not isinstance(other, Vector3D):
            raise TypeError("叉积运算需要两个Vector3D对象")
        return Vector3D(
            self.y * other.z - self.z * other.y,
            self.z * other.x - self.x * other.z,
            self.x * other.y - self.y * other.x
        )

    def __iter__(self):
        yield self.x
        yield self.y
        yield self.z


# the_3D 基向量
base_vector3D_i = Vector3D(1, 0, 0)
base_vector3D_j = Vector3D(0, 1, 0)
base_vector3D_k = Vector3D(0, 0, 1)


def create_vector3D(x: Union[int, float, de, fr, Scalar], y: Union[int, float, de, fr, Scalar], z: Union[int, float, de, fr, Scalar]) -> Vector3D:
    """创建三维向量的工厂函数"""
    return Vector3D(x, y, z)


# the_3D 矩阵类
class Matrix3D:
    """三维矩阵类（3x3）"""

    def __init__(self, data: Iterable[Vector3D] = None) -> None:
        if data is None:
            # 当data为None时，使用基向量初始化单位矩阵
            data_list = [base_vector3D_i, base_vector3D_j, base_vector3D_k]
        else:
            data_list = list(data)
            if len(data_list) != 3 or not all(isinstance(v, Vector3D) for v in data_list):
                raise TypeError('data必须是包含三个Vector3D实例的可迭代对象')
        self.rows = data_list  # 存储三行向量

    @property
    def i(self) -> Vector3D:
        return self.rows[0]

    @property
    def j(self) -> Vector3D:
        return self.rows[1]

    @property
    def k(self) -> Vector3D:
        return self.rows[2]

    def det(self) -> int | float | Decimal | Fraction | complex:
        """计算矩阵行列式"""
        # 按第一行展开
        return (
                self.i.x * (self.j.y * self.k.z - self.j.z * self.k.y) -
                self.i.y * (self.j.x * self.k.z - self.j.z * self.k.x) +
                self.i.z * (self.j.x * self.k.y - self.j.y * self.k.x)
        )

    def __mul__(self, other: Union['Matrix3D', Vector3D]) -> Union['Matrix3D', Vector3D]:
        if isinstance(other, Vector3D):
            return Vector3D(
                self.i.x * other.x + self.j.x * other.y + self.k.x * other.z,
                self.i.y * other.x + self.j.y * other.y + self.k.y * other.z,
                self.i.z * other.x + self.j.z * other.y + self.k.z * other.z
            )
        # 矩阵乘法：行×列
        row1 = Vector3D(
            self.i.dot(Vector3D(other.i.x, other.j.x, other.k.x)),
            self.i.dot(Vector3D(other.i.y, other.j.y, other.k.y)),
            self.i.dot(Vector3D(other.i.z, other.j.z, other.k.z))
        )
        row2 = Vector3D(
            self.j.dot(Vector3D(other.i.x, other.j.x, other.k.x)),
            self.j.dot(Vector3D(other.i.y, other.j.y, other.k.y)),
            self.j.dot(Vector3D(other.i.z, other.j.z, other.k.z))
        )
        row3 = Vector3D(
            self.k.dot(Vector3D(other.i.x, other.j.x, other.k.x)),
            self.k.dot(Vector3D(other.i.y, other.j.y, other.k.y)),
            self.k.dot(Vector3D(other.i.z, other.j.z, other.k.z))
        )
        return Matrix3D([row1, row2, row3])

    def __str__(self) -> str:
        return f"Matrix3D({self.i}, {self.j}, {self.k})"

    def __repr__(self) -> str:
        return f"Matrix3D(rows={self.rows!r})"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, Matrix3D):
            return False
        return self.rows == other.rows

    def __ne__(self, other: Any) -> bool:
        return not self.__eq__(other)

    def transpose(self) -> 'Matrix3D':
        """矩阵转置"""
        return Matrix3D([
            Vector3D(self.i.x, self.j.x, self.k.x),
            Vector3D(self.i.y, self.j.y, self.k.y),
            Vector3D(self.i.z, self.j.z, self.k.z)
        ])

    def __iter__(self):
        yield from self.rows


# the_3D 单位矩阵
base_Matrix3D = Matrix3D()


def create_matrix3D(rows: Union[Tuple[Vector3D, Vector3D, Vector3D], Tuple[Union[int, float, de, fr, Scalar], ...]]) -> Matrix3D:
    """创建三维矩阵的工厂函数"""
    if isinstance(rows[0], Vector3D) and isinstance(rows[1], Vector3D) and isinstance(rows[2], Vector3D):
        return Matrix3D(rows)
    elif len(rows) == 9 and all(isinstance(x, (int, float, de, fr, Scalar)) for x in rows):
        return Matrix3D([
            Vector3D(rows[0], rows[1], rows[2]),
            Vector3D(rows[3], rows[4], rows[5]),
            Vector3D(rows[6], rows[7], rows[8])
        ])
    else:
        raise TypeError("输入必须是三个Vector3D或九个Scalar的元组")


# 向量绘制功能
def draw_vector2D(vec: Vector2D, draw_speed: int = 0) -> None:
    """绘制二维向量"""
    from turtle import speed as set_speed, goto, penup, pendown, left, forward, write, home, pos, backward

    set_speed(draw_speed)
    penup()
    home()  # 回到原点
    pendown()

    # 绘制向量线
    goto(float(vec.x), float(vec.y))

    # 绘制箭头
    left(150)
    arrow_length = max(5, int(min(abs(vec.x), abs(vec.y)) / 10))  # 箭头长度自适应
    forward(arrow_length)
    penup()
    backward(arrow_length)
    left(60)
    pendown()
    forward(arrow_length)

    # 标注坐标
    penup()
    goto(float(vec.x) * 1.1, float(vec.y) * 1.1)  # 稍微偏移一点
    write(f"({vec.x}, {vec.y})")

    print(f"向量终点位置: {pos()}")
    penup()
    home()


def draw_matrix2D(mat: Matrix2D, draw_speed: int = 0) -> None:
    """绘制二维矩阵的行向量"""
    draw_vector2D(mat.i, draw_speed)
    draw_vector2D(mat.j, draw_speed)


# 1x2 矩阵类 (1行2列)
class Matrix1x2:
    """1x2矩阵类（1行2列）"""

    def __init__(self, data: Vector2D = None) -> None:
        if data is None:
            self.row = Vector2D(Scalar(0), Scalar(0))
        else:
            if not isinstance(data, Vector2D):
                raise TypeError('data必须是Vector2D实例')
            self.row = data

    def __mul__(self, other: Union[Matrix2D, Vector2D, Scalar]) -> Union['Matrix1x2', Scalar]:
        if isinstance(other, Scalar):
            return Matrix1x2(self.row * other)
        elif isinstance(other, Vector2D):
            return self.row.dot(other)
        elif isinstance(other, Matrix2D):
            return Matrix1x2(other * self.row)
        else:
            raise TypeError(f"不支持与{type(other)}类型相乘")

    def __rmul__(self, scalar: Scalar) -> 'Matrix1x2':
        return self.__mul__(scalar)

    def __str__(self) -> str:
        return f"Matrix1x2({self.row.x}, {self.row.y})"


class Matrix1x3:
    """1x3矩阵类（1行3列）"""

    def __init__(self, data: Vector3D = None) -> None:
        if data is None:
            self.row = Vector3D(Scalar(0), Scalar(0), Scalar(0))
        else:
            if not isinstance(data, Vector3D):
                raise TypeError('data必须是Vector3D实例')
            self.row = data

    def __mul__(self, other: Union[Matrix3D, Vector3D, Scalar]) -> Union['Matrix1x3', Union[int, float, Decimal, Fraction, complex]]:
        if isinstance(other, Scalar):
            return Matrix1x3(self.row * other)
        elif isinstance(other, Vector3D):
            return self.row.dot(other)
        elif isinstance(other, Matrix3D):
            return Matrix1x3(other * self.row)
        else:
            raise TypeError(f"不支持与{type(other)}类型相乘")

    def __rmul__(self, scalar: Scalar) -> 'Matrix1x3':
        return self.__mul__(scalar)

    def __str__(self) -> str:
        return f"Matrix1x3({self.row.x}, {self.row.y}, {self.row.z})"


class Matrix2x1:
    """2x1矩阵类（2行1列）"""

    def __init__(self, scalar1: Union[int, float, de, fr, Scalar], scalar2: Union[int, float, de, fr, Scalar]) -> None:
        self.rows = [
            Vector2D(scalar1, Scalar(0)),
            Vector2D(scalar2, Scalar(0))
        ]

    def __mul__(self, other: Union[Matrix1x2, Scalar]) -> Union[Matrix2D, 'Matrix2x1']:
        if isinstance(other, Scalar):
            return Matrix2x1(
                self.rows[0].x * other,
                self.rows[1].x * other
            )
        elif isinstance(other, Matrix1x2):
            row1 = Vector2D(
                self.rows[0].x * other.row.x,
                self.rows[0].x * other.row.y
            )
            row2 = Vector2D(
                self.rows[1].x * other.row.x,
                self.rows[1].x * other.row.y
            )
            return Matrix2D([row1, row2])
        else:
            raise TypeError(f"不支持与{type(other)}类型相乘")

    def __rmul__(self, scalar: Scalar) -> 'Matrix2x1':
        return self.__mul__(scalar)

    def __str__(self) -> str:
        return f"Matrix2x1(\n  {self.rows[0].x},\n  {self.rows[1].x}\n)"


class Matrix2x3:
    """2x3矩阵类（2行3列）"""

    def __init__(self, row1: Vector3D, row2: Vector3D) -> None:
        if not (isinstance(row1, Vector3D) and isinstance(row2, Vector3D)):
            raise TypeError("矩阵行必须是Vector3D实例")
        self.rows = [row1, row2]

    def __mul__(self, other: Union[Matrix3D, Vector3D, Scalar]) -> Union['Matrix2x3', Vector2D]:
        if isinstance(other, Scalar):
            return Matrix2x3(
                self.rows[0] * other,
                self.rows[1] * other
            )
        elif isinstance(other, Vector3D):
            return Vector2D(
                self.rows[0].dot(other),
                self.rows[1].dot(other)
            )
        elif isinstance(other, Matrix3D):
            return Matrix2x3(
                other * self.rows[0],
                other * self.rows[1]
            )
        else:
            raise TypeError(f"不支持与{type(other)}类型相乘")

    def __rmul__(self, scalar: Scalar) -> 'Matrix2x3':
        return self.__mul__(scalar)

    def __str__(self) -> str:
        return f"Matrix2x3(\n  {self.rows[0]},\n  {self.rows[1]}\n)"


class Matrix3x1:
    """3x1矩阵类（3行1列）"""

    def __init__(self, scalar1: Scalar, scalar2: Scalar, scalar3: Scalar) -> None:
        if not (isinstance(scalar1, Scalar) and
                isinstance(scalar2, Scalar) and
                isinstance(scalar3, Scalar)):
            raise TypeError("矩阵元素必须是Scalar类型")
        self.rows = [
            Vector3D(scalar1, Scalar(0), Scalar(0)),
            Vector3D(scalar2, Scalar(0), Scalar(0)),
            Vector3D(scalar3, Scalar(0), Scalar(0))
        ]

    def __mul__(self, other: Union[Matrix1x3, Scalar]) -> Union[Matrix3D, 'Matrix3x1']:
        if isinstance(other, Scalar):
            return Matrix3x1(
                self.rows[0].x * other,
                self.rows[1].x * other,
                self.rows[2].x * other
            )
        elif isinstance(other, Matrix1x3):
            row1 = Vector3D(
                self.rows[0].x * other.row.x,
                self.rows[0].x * other.row.y,
                self.rows[0].x * other.row.z
            )
            row2 = Vector3D(
                self.rows[1].x * other.row.x,
                self.rows[1].x * other.row.y,
                self.rows[1].x * other.row.z
            )
            row3 = Vector3D(
                self.rows[2].x * other.row.x,
                self.rows[2].x * other.row.y,
                self.rows[2].x * other.row.z
            )
            return Matrix3D([row1, row2, row3])
        else:
            raise TypeError(f"不支持与{type(other)}类型相乘")

    def __rmul__(self, scalar: Scalar) -> 'Matrix3x1':
        return self.__mul__(scalar)

    def __str__(self) -> str:
        return f"Matrix3x1(\n  {self.rows[0].x},\n  {self.rows[1].x},\n  {self.rows[2].x}\n)"


class Matrix3x2:
    """3x2矩阵类（3行2列）"""

    def __init__(self, data: Iterable[Vector2D] = None) -> None:
        """
        初始化3x2矩阵
        :param data: 包含3个Vector2D实例的可迭代对象，每个Vector2D代表一行
        """
        if data is None:
            # 默认创建单位矩阵的3x2形式
            data_list = [
                Vector2D(Scalar(1), Scalar(0)),
                Vector2D(Scalar(0), Scalar(1)),
                Vector2D(Scalar(0), Scalar(0))
            ]
        else:
            data_list = list(data)
            if len(data_list) != 3 or not all(isinstance(v, Vector2D) for v in data_list):
                raise TypeError('data必须是包含三个Vector2D实例的可迭代对象')
        self.rows = data_list

    @property
    def row1(self) -> Vector2D:
        return self.rows[0]

    @property
    def row2(self) -> Vector2D:
        return self.rows[1]

    @property
    def row3(self) -> Vector2D:
        return self.rows[2]

    def __mul__(self, other: Union[Scalar, Vector2D, Matrix2D]) -> Union['Matrix3x2', Vector3D]:
        """
        3x2矩阵乘法
        支持与标量、2D向量和2x2矩阵相乘
        """
        if isinstance(other, Scalar):
            # 与标量相乘：每个元素都乘以该标量
            return Matrix3x2([row * other for row in self.rows])

        elif isinstance(other, Vector2D):
            # 与2D向量相乘：返回3D向量
            return Vector3D(
                self.row1.dot(other),
                self.row2.dot(other),
                self.row3.dot(other)
            )

        elif isinstance(other, Matrix2D):
            # 与2x2矩阵相乘：返回3x2矩阵
            # 计算新矩阵的每一行
            new_row1 = Vector2D(
                self.row1.x * other.row1.x + self.row1.y * other.row2.x,
                self.row1.x * other.row1.y + self.row1.y * other.row2.y
            )
            new_row2 = Vector2D(
                self.row2.x * other.row1.x + self.row2.y * other.row2.x,
                self.row2.x * other.row1.y + self.row2.y * other.row2.y
            )
            new_row3 = Vector2D(
                self.row3.x * other.row1.x + self.row3.y * other.row2.x,
                self.row3.x * other.row1.y + self.row3.y * other.row2.y
            )
            return Matrix3x2([new_row1, new_row2, new_row3])

        else:
            raise TypeError(f"不支持3x2矩阵与{type(other)}类型相乘")

    def __rmul__(self, scalar: Scalar) -> 'Matrix3x2':
        """支持标量在左侧的乘法（如 scalar * matrix）"""
        return self.__mul__(scalar)

    def __str__(self) -> str:
        return f"Matrix3x2(\n  {self.row1},\n  {self.row2},\n  {self.row3}\n)"


# 工厂函数：创建各种类型的矩阵
def create_matrix1x2(x: Scalar, y: Scalar) -> Matrix1x2:
    """创建1x2矩阵的工厂函数"""
    return Matrix1x2(Vector2D(x, y))


def create_matrix1x3(x: Scalar, y: Scalar, z: Scalar) -> Matrix1x3:
    """创建1x3矩阵的工厂函数"""
    return Matrix1x3(Vector3D(x, y, z))


def create_matrix2x1(a: Scalar, b: Scalar) -> Matrix2x1:
    """创建2x1矩阵的工厂函数"""
    return Matrix2x1(a, b)


def create_matrix2x3(row1: Vector3D, row2: Vector3D) -> Matrix2x3:
    """创建2x3矩阵的工厂函数"""
    return Matrix2x3(row1, row2)


def create_matrix3x1(a: Scalar, b: Scalar, c: Scalar) -> Matrix3x1:
    """创建3x1矩阵的工厂函数"""
    return Matrix3x1(a, b, c)


def create_matrix3x2(row1: Vector2D, row2: Vector2D, row3: Vector2D) -> Matrix3x2:
    """创建3x2矩阵的工厂函数"""
    return Matrix3x2(row1, row2, row3)