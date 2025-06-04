import re
import os
import sys
import fitz
import sqlite3

class PdfDocument(fitz.Document):
    def __init__(self, filename):
        # 直接调用父类构造函数
        super().__init__(filename)

    def build_reverse_index(self):
        text_list = []
        for i, page in enumerate(self):
            text = page.get_text()
            for line in text.split("\n"):
                # more than 1 char
                line = line.strip()
                if len(line) > 1:
                    text_list.append(f"{i + 1}: {line}")
        return "\n".join(text_list)

def getDirFiles(dir):
    fileList = []
    for ff in os.listdir(dir):
        # 过滤隐藏文件夹
        if ff.startswith('.'):
            continue
        filePath = os.path.join(dir, ff)
        if os.path.isdir(filePath):
            fileList.extend(getDirFiles(filePath))
        else:
            if ff.lower().endswith('.pdf'):
                fileList.append(os.path.join(dir, ff))
    return fileList

def init_database():
    """初始化数据库和表结构"""
    db_path = 'pdf-annotations.db'
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # 创建 files 表（如果不存在）
    cursor.execute('''
        CREATE TABLE IF NOT EXISTS files (
            file TEXT UNIQUE PRIMARY KEY,
            title TEXT NOT NULL
        )
    ''')

    conn.commit()
    conn.close()
    return db_path

def insert_file_record(db_path, file_path, title):
    """向数据库插入文件记录"""
    try:
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()

        # 检查是否已存在相同记录
        cursor.execute('SELECT file FROM files WHERE file = ? AND title = ?', (file_path, title))
        if cursor.fetchone():
            print(f"数据库中已存在记录: {title}")
            conn.close()
            return

        # 插入新记录
        cursor.execute('INSERT INTO files (file, title) VALUES (?, ?)', (file_path, title))
        conn.commit()
        print(f"已添加到数据库: {title} -> {file_path}")

    except Exception as e:
        print(f"数据库操作错误: {e}")
    finally:
        conn.close()

def process_pdf(pdf_path, cache_dir, db_path):
    """处理单个PDF文件，提取文本并保存到Cache目录"""
    try:
        # 生成输出文件路径
        pdf_name = os.path.basename(pdf_path)
        txt_name = os.path.splitext(pdf_name)[0] + '.txt'
        output_path = os.path.join(cache_dir, txt_name)

        # 如果 pdf_name 不存在，将信息写入数据库
        insert_file_record(db_path, os.path.abspath(pdf_name), pdf_name)

        # 如果已经处理过，跳过
        if os.path.exists(output_path):
            print(f"跳过已处理的文件: {pdf_name}")
            return True

        print(f"处理文件: {pdf_name}")

        # 打开PDF文档
        doc = PdfDocument(pdf_path)

        # 构建反向索引
        text_content = doc.build_reverse_index()

        # 保存到txt文件
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(text_content)

        # 关闭文档
        doc.close()

        print(f"完成处理: {pdf_name} -> {txt_name}")
        return True

    except Exception as e:
        print(f"处理文件 {pdf_path} 时出错: {e}")
        return False

if __name__ == '__main__':
    # 初始化数据库
    db_path = init_database()
    print(f"数据库初始化完成: {db_path}")

    # 创建Cache目录
    cache_dir = 'Cache'
    if not os.path.exists(cache_dir):
        os.makedirs(cache_dir)
        print(f"创建 Cache 目录: {cache_dir}")

    if len(sys.argv) < 2:
        # 没有参数，处理当前目录下的所有PDF文件
        pdf_files = getDirFiles('.')
        print(f"找到 {len(pdf_files)} 个PDF文件")
    else:
        # 处理指定的文件
        pdf_files = [f for f in sys.argv[1:] if f.lower().endswith('.pdf')]
        print(f"指定处理 {len(pdf_files)} 个PDF文件")

    if not pdf_files:
        print("没有找到PDF文件")
        sys.exit(1)

    success_count = 0
    fail_count = 0

    for pdf_file in pdf_files:
        if process_pdf(pdf_file, cache_dir, db_path):
            success_count += 1
        else:
            fail_count += 1

    print(f"\n处理完成! 成功: {success_count}, 失败: {fail_count}")
